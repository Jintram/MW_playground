


aapje=tryCatch(
    expr = {
        oeleboele=4444
        FLAG=F
        sdkljfh9889sdf=sdf=sad=fsad=faaaaaaa
    },
    error = function(e){ 
        #warning('Stuff went wrong 2')
        print('Stuff went wrong')
        FLAG=T
    },
    warning = function(w){
        print('Warning activated')
        # (Optional)
        # Do this if an warning is caught...
    },
    finally = {
        # (Optional)
        # Do this at the end before quitting the tryCatch structure...
    }
)
