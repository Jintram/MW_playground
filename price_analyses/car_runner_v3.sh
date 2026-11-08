#!/bin/bash

# Cars of interest
#
# Model	            Heupruimte achter	Schouderruimte achter
# Polestar 2	    135,4 cm	        133,6 cm
# Ford Mach-E	    135,1 cm	        142,0 cm
# Nissan Ariya	    133,4 cm	        142,0 cm
# Tesla Model 3	    133,1 cm	        137,2 cm
# Hyundai Kona	    132,6 cm	        138,5 cm
# Kia e-Niro	    122,7 cm (?)	    140,0 cm


python extracting_car_data_v3claude.py "https://www.anwb.nl/auto/kopen/zoeken/merk/nissan/model/ariya"
python analyzing_car_data_v3claude.py /Users/m.wehrens/Documents/Personal/auto/2026/cardata_anwb_nissan_ariya_2026-11-08.xlsx

python extracting_car_data_v3claude.py https://www.anwb.nl/auto/kopen/zoeken/merk/ford/model/mustang-mach-e
python analyzing_car_data_v3claude.py /Users/m.wehrens/Documents/Personal/auto/2026/cardata_anwb_ford_mustang-mach-e_2026-11-08.xlsx

python extracting_car_data_v3claude.py https://www.anwb.nl/auto/kopen/zoeken/merk/hyundai/model/kona
python analyzing_car_data_v3claude.py 