"""
Extract used-car listings from anwb.nl into an Excel file.

Usage (command line):
    python extracting_car_data_v3claude.py "https://www.anwb.nl/auto/kopen/zoeken/merk/kia/model/niro-ev"
    python extracting_car_data_v3claude.py <url> --no-details      # faster, search results only
    python extracting_car_data_v3claude.py <url> --max-cars 20     # quick test

Or interactively (e.g. VS Code cells): set SEARCH_URL below and run the file.

Where the Excel file goes
-------------------------
By default NOT in the current directory, but in:
    OUTPUTDIR/<current year>/cardata_anwb_<brand>_<model>_<date>.xlsx
    e.g. /Users/m.wehrens/Documents/Personal/auto/2026/cardata_anwb_kia_niro-ev_2026-11-08.xlsx
(OUTPUTDIR is set below). Use  -o <path.xlsx>  to choose the file yourself, or --outputdir <dir>.
The script prints the full path when it is done ("Saved N cars to ...").

How it works
------------
The ANWB "occasion zoeken" page doesn't contain the car data in its HTML; a JavaScript
app loads it from ANWB's own JSON API (the same calls your browser makes,
including the "laad de volgende 30 resultaten" button):

  POST {API}/listings/search-with-counters?pageSize=..&pageNumber=..   -> list of cars
  GET  {API}/listings/fetch/{advertisementId}                          -> one car's detail page

The script calls this API directly, which is much faster and more robust than parsing HTML.
The API key is the public key in ANWB's frontend JavaScript. If it ever stops working, the
script looks up the current key in that JavaScript automatically.

Output columns
--------------
The first columns match the old manual Excel (cardata_manual_2024_v1.xlsx):
    price, KM (x1000 km), year, special, model, ref_year
followed by many more fields (edition, first registration, power, range, seller, links, ...).
A second sheet 'all_fields' has every field the API returns (flattened), and a 'SOURCE'
sheet records the search URL and date.
"""

import argparse
import datetime as dt
import json
import os
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

import pandas as pd

########################################################################
# Settings (used when running interactively without command-line arguments)

SEARCH_URL = 'https://www.anwb.nl/auto/kopen/zoeken/merk/kia/model/niro-ev'
OUTPUTDIR = '/Users/m.wehrens/Documents/Personal/auto/'

########################################################################
# Constants

API_BASE = 'https://api.anwb.nl/occasion-portal/v1/occasion-portal'
API_KEY_DEFAULT = 'wQgdJZbpjCxuM5wh2kSxPTjzE9FVUaye'  # public key from ANWB's frontend JS (prod)
APP_ENTRY_JS = 'https://7143.static.anwb.nl/index.js'  # ANWB occasion search app
DETAIL_URL_TEMPLATE = 'https://www.anwb.nl/auto/kopen/detail/merk={brand}/model={model}/overzicht/{id}'
USER_AGENT = 'Mozilla/5.0 (car price analysis script)'

PAGE_SIZE = 100  # the API accepts up to 200; the website uses 30

########################################################################
# HTTP helpers

_api_key = API_KEY_DEFAULT


def _http(url, data=None, headers=None, method=None, retries=6):
    """Simple HTTP request with retries; returns the decoded body as text.
    On '429 Too Many Requests' it waits (Retry-After header, else 5, 10, 20, 40, 60 s) and retries."""
    headers = dict(headers or {})
    headers.setdefault('User-Agent', USER_AGENT)
    body = json.dumps(data).encode() if data is not None else None
    for attempt in range(retries):
        req = urllib.request.Request(url, data=body, headers=headers, method=method)
        wait = min(5 * 2 ** attempt, 60)
        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                return resp.read().decode('utf-8')
        except urllib.error.HTTPError as e:
            if e.code in (401, 403, 404) or attempt == retries - 1:
                raise
            retry_after = e.headers.get('Retry-After') if e.headers else None
            if retry_after and retry_after.isdigit():
                wait = int(retry_after) + 1
            if e.code == 429:
                print(f'  rate limited (429), waiting {wait} s ...')
        except urllib.error.URLError:
            if attempt == retries - 1:
                raise
        time.sleep(wait)


def discover_api_key():
    """Find the current production API key in ANWB's frontend JavaScript."""
    entry = _http(APP_ENTRY_JS)
    chunks = re.findall(r'chunk-[a-z0-9]+\.js', entry)
    base = APP_ENTRY_JS.rsplit('/', 1)[0] + '/assets/'
    seen = set()
    while chunks:
        c = chunks.pop()
        if c in seen:
            continue
        seen.add(c)
        js = _http(base + c)
        m = re.search(r'prod:\{endpoint:"[^"]*occasion-portal[^"]*",apiKey:(\w+)\}', js)
        if m:
            var = m.group(1)
            m2 = re.search(r'\b' + re.escape(var) + r'="([A-Za-z0-9]+)"', js)
            if m2:
                return m2.group(1)
        chunks += re.findall(r'chunk-[a-z0-9]+\.js', js)
    raise RuntimeError('Could not find the ANWB API key in the website JavaScript.')


def api_call(path, data=None, params=None, method='GET'):
    """Call the ANWB occasion API; refreshes the API key once if it is rejected."""
    global _api_key
    url = API_BASE + path
    if params:
        url += '?' + urllib.parse.urlencode(params)
    for attempt in range(2):
        headers = {'apiKey': _api_key, 'Content-Type': 'application/json'}
        try:
            return json.loads(_http(url, data=data, headers=headers, method=method))
        except urllib.error.HTTPError as e:
            if e.code in (401, 403) and attempt == 0:
                print('API key rejected, looking up the current key on anwb.nl ...')
                _api_key = discover_api_key()
                continue
            raise


########################################################################
# Parsing the search URL

# URL query parameter -> API field, as defined in ANWB's search app JavaScript
URL_MULTISELECT_SLUG = {   # e.g. ?brandstof[0]=elektriciteit&brandstof[1]=hybride
    'brandstof': 'fuelType', 'transmissie': 'transmissionType', 'carrosserie': 'bodyType',
    'kleur': 'color', 'uitrusting': 'searchOptions', 'zekerheden': 'guaranteeType',
    'energielabel': 'energyLabel',
}
URL_MULTISELECT_INT = {'aantal-deuren': 'numberOfDoors', 'aantal-zitplaatsen': 'numberOfSeats'}
URL_RANGE = {'prijs': 'price', 'kilometerstand': 'mileage'}   # e.g. ?prijs=20000-30000
URL_FROM_UNTIL = {   # e.g. ?vanaf-bouwjaar=2021&tot-en-met-bouwjaar=2023
    'bouwjaar': 'constructionYear', 'vermogen': 'power', 'aanhangwagenmassa': 'towingWeight',
}


def _to_int(s):
    return int(re.sub(r'[^\d]', '', s)) if re.search(r'\d', s or '') else None


def parse_search_url(url):
    """
    Turn an anwb.nl search URL into an API filter, the way the website does it.
    Path:   .../zoeken/merk/<brand>[/model/<model>]
    Query:  brandstof[0]=..., carrosserie[0]=..., kleur[0]=..., transmissie[0]=..., energielabel[0]=...,
            prijs=<min>-<max>, kilometerstand=<min>-<max>, vanaf-/tot-en-met-bouwjaar=<year>,
            vanaf-/tot-en-met-vermogen=<pk>, postcode-en-afstand=<postcode>-<km>km, ...
    Unknown query parameters are reported and ignored.
    """
    parsed = urllib.parse.urlparse(url)
    m_brand = re.search(r'/merk/([^/?]+)', parsed.path)
    m_model = re.search(r'/model/([^/?]+)', parsed.path)
    if not m_brand:
        raise ValueError('Could not find /merk/<brand> in the URL: ' + url)
    brand_filter = {'slug': m_brand.group(1)}
    if m_model:
        brand_filter['models'] = [{'slug': m_model.group(1)}]
    filters = {'brandModel': {'filter': [brand_filter]}}

    # collect query values per parameter, merging 'brandstof[0]', 'brandstof[1]', ... into 'brandstof'
    params = {}
    for key, value in urllib.parse.parse_qsl(parsed.query):
        params.setdefault(re.sub(r'\[\d*\]$', '', key.lower()), []).append(value)

    from_until = {}
    for key, values in params.items():
        if key in URL_MULTISELECT_SLUG:
            slugs = [v for val in values for v in val.split(',') if v]
            filters[URL_MULTISELECT_SLUG[key]] = {'filter': [{'slug': s} for s in slugs]}
        elif key in URL_MULTISELECT_INT:
            nums = [_to_int(v) for val in values for v in val.split(',') if _to_int(v) is not None]
            filters[URL_MULTISELECT_INT[key]] = {'filter': [{'name': n} for n in nums]}
        elif key in URL_RANGE:
            parts = values[0].split('-') if len(values) == 1 else values[:2]
            lo, hi = (_to_int(parts[0]), _to_int(parts[1]) if len(parts) > 1 else None)
            rng = {}
            if lo is not None:
                rng['min'] = [{'name': lo}]
            if hi is not None:
                rng['max'] = [{'name': hi}]
            filters[URL_RANGE[key]] = rng
        elif re.match(r'(vanaf|tot-en-met)-', key) and key.split('-')[-1] in URL_FROM_UNTIL:
            api_key = URL_FROM_UNTIL[key.split('-')[-1]]
            bound = 'min' if key.startswith('vanaf') else 'max'
            from_until.setdefault(api_key, {})[bound] = [{'name': _to_int(values[0])}]
        elif key == 'postcode-en-afstand':   # e.g. 3584-25km
            postcode, _, dist = values[0].partition('-')
            filters['location'] = {'postalCode': [{'name': postcode}]}
            if _to_int(dist):
                filters['location']['distance'] = [{'name': _to_int(dist)}]
        else:
            print(f'WARNING: URL filter "{key}={values}" is not supported and is ignored.')
    filters.update(from_until)
    return filters


def describe_filters(filters):
    """Short text for the file name, e.g. 'hyundai_kona_elektriciteit'."""
    f = filters['brandModel']['filter'][0]
    parts = [f['slug']] + ([f['models'][0]['slug']] if 'models' in f else [])
    for key, val in filters.items():
        if key == 'brandModel':
            continue
        if 'filter' in val:
            parts.append('-'.join(str(x.get('slug', x.get('name'))) for x in val['filter']))
        elif 'min' in val or 'max' in val:
            lo = val.get('min', [{}])[0].get('name', '')
            hi = val.get('max', [{}])[0].get('name', '')
            parts.append(f'{key}{lo}-{hi}')
        elif key == 'location':
            parts.append('pc' + str(val['postalCode'][0]['name']))
    return re.sub(r'[^\w.-]+', '-', '_'.join(parts))


########################################################################
# Fetching

def fetch_all_listings(filters, page_size=PAGE_SIZE, max_cars=None, delay=0.3):
    """Fetch all search results (equivalent to clicking 'laad de volgende 30 resultaten' until done)."""
    hits, page, total = [], 1, None
    while True:
        res = api_call('/listings/search-with-counters', data=filters, method='POST',
                       params={'pageSize': page_size, 'pageNumber': page,
                               'sortField': 'relevance', 'sortOrder': 'asc'})
        if res.get('errors'):
            raise RuntimeError(f'API returned errors: {res["errors"]}')
        total = res['total']
        new = res.get('hits', [])
        hits += new
        print(f'  page {page}: {len(hits)}/{total} cars')
        if not new or len(hits) >= total or (max_cars and len(hits) >= max_cars):
            break
        page += 1
        time.sleep(delay)
    # de-duplicate (listing order can shift slightly while paging)
    uniq = {h['advertisementId']: h for h in hits}
    hits = list(uniq.values())
    if max_cars:
        hits = hits[:max_cars]
    return hits, total


def fetch_details(ids, delay=0.3):
    """Fetch detail records; failed ones are retried once more at the end (after a pause)."""
    details, failed = {}, []
    for i, ad_id in enumerate(ids, 1):
        try:
            details[ad_id] = api_call(f'/listings/fetch/{ad_id}')
        except Exception as e:  # e.g. listing removed in the meantime, or persistent rate limit
            print(f'  could not fetch details for {ad_id}: {e} (will retry at the end)')
            failed.append(ad_id)
        if i % 25 == 0 or i == len(ids):
            print(f'  details {i}/{len(ids)}')
        time.sleep(delay)
    if failed:
        print(f'Retrying {len(failed)} failed detail pages after 30 s ...')
        time.sleep(30)
        for ad_id in failed:
            try:
                details[ad_id] = api_call(f'/listings/fetch/{ad_id}')
            except Exception as e:
                print(f'  still no details for {ad_id}: {e} -> row has search-result fields only')
            time.sleep(delay * 3)
    n_missing = len(ids) - len(details)
    print(f'Details fetched for {len(details)}/{len(ids)} cars' +
          (f' ({n_missing} rows have search-result fields only)' if n_missing else ''))
    return details


########################################################################
# Converting to a table

def _get(d, *keys, default=None):
    for k in keys:
        if not isinstance(d, dict) or k not in d:
            return default
        d = d[k]
    return d


def _from_comments(comments, label):
    """Pull 'Label: **value**' out of the seller's description (markdown), if present."""
    if not comments:
        return None
    m = re.search(re.escape(label) + r':\s*\*\*(.+?)\*\*', comments)
    return m.group(1).replace('\\', '').strip() if m else None


def _to_number(s):
    if s is None:
        return None
    m = re.search(r'[\d]+(?:[.,]\d+)?', s)
    return float(m.group(0).replace(',', '.')) if m else None


def _title_number(general, pattern):
    """Regex a number out of the listing title (edition), e.g. '64 kWh' or 'SOH 98%'."""
    title = (general.get('commercialEdition') or general.get('editon') or '').lower()
    m = re.search(pattern, title)
    return float(m.group(1).replace(',', '.')) if m else None


def _date(s):
    return s[:10] if isinstance(s, str) else None


def make_row(hit, detail, ref_year):
    car = detail or hit  # the detail record has the most fields; fall back to the search hit
    g = _get(car, 'carData', 'general', default={})
    h = _get(car, 'carData', 'history', default={})
    mw = _get(car, 'carData', 'measuresAndWeights', default={})
    eng = _get(car, 'carData', 'engine', default={})
    env = _get(car, 'carData', 'environment', default={})
    perf = _get(car, 'carData', 'performance', default={})
    seller = _get(car, 'seller', 'sellerData', default={})
    price = _get(car, 'price', 'totalIncludingTax') or _get(hit, 'price', 'totalIncludingTax') \
        or _get(hit, 'price', 'amount')
    mileage = h.get('mileage')
    comments = (detail or {}).get('comments')

    row = {
        # --- same columns as the old manual Excel ---
        'price': price,
        'KM': mileage / 1000 if mileage is not None else None,   # thousands of km, as before
        'year': h.get('constructionYear'),
        'special': None,
        'model': g.get('modelSlug'),
        'ref_year': ref_year,
        # --- extra fields ---
        'mileage_km': mileage,
        'brand': g.get('brandName'),
        'model_name': g.get('modelName'),
        'edition': g.get('commercialEdition') or g.get('editon'),
        'first_registration': _date(h.get('dateFirstAdmissionInternational')),
        'first_registration_NL': _date(h.get('dateFirstAdmissionNational')),
        'catalog_price': h.get('catalogPrice'),
        'fuel': g.get('fuelType'),
        'transmission': g.get('transmissionType'),
        'body': g.get('bodyType'),
        'color': g.get('color'),
        'seats': g.get('numberOfSeats'),
        'power_hp': eng.get('powerHp'),
        'power_kw': eng.get('powerKw'),
        'range_km': mw.get('actionRadius'),
        'battery_kWh_title': _title_number(g, r'(\d+(?:[.,]\d+)?)\s*kwh'),
        'battery_usable_kWh': _to_number(_from_comments(comments, 'Bruikbare accucapaciteit')),
        'battery_SOH_pct_title': _title_number(g, r'(?:soh|accu)\s*:?\s*(\d+(?:[.,]\d+)?)\s*%'),
        'consumption_kWh_100km': _to_number(_from_comments(comments, 'Gemiddeld elektriciteitsverbruik')),
        'number_of_owners': _to_number(_from_comments(comments, 'Aantal eigenaren')),
        'factory_warranty_until': _from_comments(comments, 'Fabrieksgarantie tot'),
        'heat_pump': ('warmtepomp' in (comments or '').lower() + (g.get('commercialEdition') or '').lower())
                     if detail else None,
        'acceleration_0_100': perf.get('accelerationTo100'),
        'energy_label': env.get('energyLabel'),
        'empty_weight': mw.get('emptyWeight'),
        'trunk_l': mw.get('trunkCapacityMinimal'),
        'road_tax_min': mw.get('minimumRoadTax'),
        'road_tax_max': mw.get('maximumRoadTax'),
        'apk_expires': _date(h.get('expireApk')),
        'first_owner': h.get('firstOwner'),
        'mileage_correct': h.get('mileageCorrect'),
        'imported': h.get('imported'),
        'maintenance_log': (detail or {}).get('maintenanceLogPresent'),
        'license_plate': car.get('licencePlate'),
        'seller': seller.get('name'),
        'seller_city': seller.get('city'),
        'seller_province': seller.get('province'),
        'date_added': _date(car.get('dateAdded')),
        'advertisementId': car.get('advertisementId'),
        'anwb_url': DETAIL_URL_TEMPLATE.format(brand=g.get('brandSlug'), model=g.get('modelSlug'),
                                               id=car.get('advertisementId')),
        'seller_url': (detail or {}).get('externalUrl'),
        'autotrack_url': car.get('url'),
    }
    return row


def flatten_all(hit, detail):
    """All fields, flattened (for the 'all_fields' sheet). Drops bulky photo/option lists."""
    rec = dict(detail or hit)
    for k in ('media', 'options', 'comments', 'propositions', 'guaranteesAndCertainties'):
        rec.pop(k, None)
    flat = pd.json_normalize(rec, sep='.').iloc[0].to_dict()
    return {k: (json.dumps(v, ensure_ascii=False) if isinstance(v, (list, dict)) else v)
            for k, v in flat.items()}


########################################################################
# Main

def extract(search_url, outputdir=OUTPUTDIR, details=True, max_cars=None, delay=0.3, outfile=None):
    today = dt.date.today()
    filters = parse_search_url(search_url)
    print(f'Searching: {search_url}')
    print(f'API filter: {json.dumps(filters)}')
    hits, total = fetch_all_listings(filters, max_cars=max_cars, delay=delay)
    print(f'Found {len(hits)} cars (site reports {total}).')

    det = {}
    if details:
        print('Fetching detail pages ...')
        det = fetch_details([h['advertisementId'] for h in hits], delay=delay)

    rows = [make_row(h, det.get(h['advertisementId']), today.year) for h in hits]
    df = pd.DataFrame(rows).sort_values(['year', 'mileage_km'], ascending=[False, True])
    df_all = pd.DataFrame([flatten_all(h, det.get(h['advertisementId'])) for h in hits])
    df_source = pd.DataFrame({'key': ['search_url', 'date_extracted', 'n_cars', 'details_fetched'],
                              'value': [search_url, today.isoformat(), len(df), details]})

    if outfile is None:
        name = describe_filters(filters)
        outfile = os.path.join(outputdir, str(today.year), f'cardata_anwb_{name}_{today:%Y-%m-%d}.xlsx')
    os.makedirs(os.path.dirname(outfile), exist_ok=True)
    with pd.ExcelWriter(outfile) as writer:
        df.to_excel(writer, sheet_name='Sheet1', index=False)
        df_all.to_excel(writer, sheet_name='all_fields', index=False)
        df_source.to_excel(writer, sheet_name='SOURCE', index=False)
    print(f'Saved {len(df)} cars to {outfile}')
    return df


def main(argv=None):
    p = argparse.ArgumentParser(description='Extract car listings from an anwb.nl search URL into Excel.')
    p.add_argument('url', nargs='?', default=SEARCH_URL, help='anwb.nl search URL (…/zoeken/merk/<brand>/model/<model>)')
    p.add_argument('-o', '--outfile', default=None, help='output .xlsx path (default: OUTPUTDIR/<year>/cardata_anwb_<brand>_<model>_<date>.xlsx)')
    p.add_argument('--outputdir', default=OUTPUTDIR)
    p.add_argument('--no-details', action='store_true', help='skip per-car detail pages (faster, fewer columns)')
    p.add_argument('--max-cars', type=int, default=None, help='limit number of cars (for testing)')
    p.add_argument('--delay', type=float, default=0.3, help='seconds between requests (be polite)')
    a = p.parse_args(argv)
    return extract(a.url, outputdir=a.outputdir, details=not a.no_details, max_cars=a.max_cars,
                   delay=a.delay, outfile=a.outfile)


if __name__ == '__main__':
    # In an interactive session (e.g. VS Code "Run cell") sys.argv contains kernel args; ignore them.
    df = main([] if hasattr(sys, 'ps1') or 'ipykernel' in sys.modules else None)
