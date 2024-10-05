rule download_fasta:
    output:
        "{localreffolder}{fafile_mm}" if species == "mouse" else "{localreffolder}{fafile_hs}"
    params:
        species=lambda wildcards: "human" if wildcards.species == "human" else "mouse",
        url="{loc_mm}{fafile_mm}" if species == "mouse" else "{loc_hs}{fafile_hs}",
        wheretoputit="{localreffolder}{fafile_mm}" if species == "mouse" else "{localreffolder}{fafile_hs}"
    shell:
        "wget {params.url} -O {params.wheretoputit}"