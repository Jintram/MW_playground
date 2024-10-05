
# Yet another snakemake file

outputfolder="/Users/m.wehrens/temp/"

rule all:
    input: 
        "{a}_{b}.log"

rule dummyfile:
    output:
        "{a}_{b}.log"
    shell:
        "touch {wildcards.a}_{wildcards.b}.log"
        # note that the syntax is somewhat confusing here,
        # as in the "shell" "{wildcards.a}" needs to be specified
        # whereas elsewhere, you can simply use {a}

