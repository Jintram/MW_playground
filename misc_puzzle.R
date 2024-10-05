library(ggplot2)

myletters=c('W', 'P', 'R', 'D', 'Q', 'T', 'O', 'F', 'G', 'O', 'D', 'I', 'Q', 'R', 'O', 'K', 'G', 'P', 'I', 'O', 'O', 'R', 'V', 'P', 'I', 'J', 'I', 'Q', 'Z', 'J', 'I', 'I', 'O', 'Q', 'R', 'O', 'G', 'Q', 'O', 'G')

View(as.data.frame(table(myletters)))


mysentence=c('W', 'P', 'R', ' ', # wat
            'D', 'Q',  ' ', # is
            'T', 'O',  ' ', # de
            'F', 'G', 'O', 'D', 'I', 'Q', 'R', 'O',  ' ', # kleinste
            'K', 'G', 'P', 'I', 'O', 'O', 'R',  ' ', # planeet
            'V', 'P', 'I',  ' ', # van
            'J', 'I', 'Q',  ' ', # het
            'Z', 'J', 'I', 'I', 'O', 'Q', 'R', 'O', 'G', 'Q', 'O', 'G') # zonnestelsel)

subst = c('W'='W',
          'P'='A',
          'R'='T',
          'D'='I',
          'Q'='S',
          'T'='D',
          'O'='E',
          ' '=' ',
          'K'='P', 'G'='L', 'I'='N',  'R'='T',
          'Z'='Z', 'J'='O',
          'F'='K', 'V'='V'
          )

mytmp=subst[mysentence]
mytmp[is.na(mytmp)]='.'
paste0(mytmp,collapse='')
                   
subst[order(names(subst))]

substinv=names(subst)
names(substinv)=subst

abc=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
mytmp=substinv[abc]
names(mytmp) = abc
mytmp[is.na(mytmp)]='.'
mytmp

View(as.matrix(t(data.frame(origineel=names(mytmp), vercijferd=mytmp))))


##########

EXTRA_ASSUMPTIONS = c()
                        # c('D'='W')#,'T'='L','H'='K')
                      # Starts of lines are:
                      # DGTHKKPTMH
                      # .E........
                      # WE........
                      # BFLMEKKEVM
                      # ..........
                      #
                      ## DKGHM is start of question
                                       # ..E..
                                       # HOE
                                       # _or_
                                       # DGTHKK
                                       # .E....
                                        # furthermore ..
                                        # WELKAA.LOK.
                                        # DGTHKKPTMHSGG


library(stringr)
X='DGTHKKPTMHSGGJVUURTCGDGTHKKPTUBTEGDMJE_DKGHMJLZMHDGTEURGJVKKREMTBFIGARMJE?BFLMEKKEVMJVGRHMJLGJIMMRHGGOGRVMJKP_BFWBHEMTDGTGBJEBLTIMMRJBGTEMTDGTSTKPT'
myInput2 = as.vector(str_split_fixed(X,'',n=nchar(X)))

#View(as.data.frame(table(myInput2)))

subst2 = c(c('G'='E','?'='?','_'='_'), EXTRA_ASSUMPTIONS)

# Just to show where the E's are
mytmp=c('G'='E','?'='?','_'='_')[myInput2]
mytmp[is.na(mytmp)]='.'
paste0(mytmp,collapse='')

if (F) {
    # to test completely custom guess
    subst2b = c('_'='_','G'='E','?'='?','K'='O')
    mytmp=subst2b[myInput2]
    mytmp[is.na(mytmp)]='.'
    paste0(mytmp,collapse='')
}
# M=A? NOT: T=A, J=A, POSSIBLY: K=A

# these letters (most occuring in encrypted)
# MTJKE # K occurs often double, perhaps an O or N?
        # T occurs in first word likely (pos3), 
# correspond to (decrypted letters)
# NATIROD

most_occuring_in_question = c('M', 'T', 'J', 'K', 'E')
frequently_occuring_letters = c('N', 'A', 'T', 'I', 'R', 'O', 'D')


# this is a bit dirty
additional_remove_count = sum(names(subst_test) %in% names(EXTRA_ASSUMPTIONS)) 
most_occuring_in_question = most_occuring_in_question[!(most_occuring_in_question %in% names(EXTRA_ASSUMPTIONS))]
frequently_occuring_letters = frequently_occuring_letters[!(frequently_occuring_letters %in% EXTRA_ASSUMPTIONS)]

# generate all combis

all_combs=combinat::permn(frequently_occuring_letters)
all_combs2=lapply(all_combs, function(S){S[1:(5-additional_remove_count)]})
all_combs3 = unique(all_combs2)

subst_test = all_combs2[[1]]
names(subst_test) = 


key_list=list()
bruteforcelist = list()
for (idx in 1:length(all_combs3)) {
    
    subst_test = all_combs3[[idx]]
    names(subst_test) = most_occuring_in_question

    subst3 = c(subst2, subst_test)
    
    mytmp=subst3[myInput2]
    mytmp[is.na(mytmp)]='.'
    bruteforcelist[[idx]]=paste0(mytmp,collapse='')
    
    key_list[[idx]]=subst3
    
}

# View(data.frame(x=unlist(bruteforcelist)))

# we only have NATIRODE
bruteforstlist2 = unlist(bruteforcelist)
length(bruteforstlist2)
hits=    
        grepl(pattern = 'EEN', x=bruteforstlist2)&
        #grepl(pattern = '^WELKAA.LOK', x=bruteforstlist2)&
        (!grepl(pattern = 'IDDI|IOOI|IRRI|TRRT|DTTD|TDDT', x=bruteforstlist2))&
        (!grepl(pattern = '_.AE|_WAE|_WOE', x=bruteforstlist2))&
        # (!grepl(pattern = '^WEO|^WE..(NN|RR|DD|TT)', x=bruteforstlist2))&  # |^WELKRR|^WELKDD|^WELKTT
        (!grepl(pattern = '^.EO|^.ED|^.EA|^.EI', x=bruteforstlist2))&
        (!grepl(pattern = 'LDK|LRK', x=bruteforstlist2))&
        (!grepl(pattern = 'HRNA_|^\\.ED|II', x=bruteforstlist2))&
        (!grepl(pattern = 'ANO\\?|RNO\\?|DNR\\?|DNT\\?|ANR\\?DNI\\?|DNA\\?|ATI\\?|NIA\\?|NID\\?|RNT\\?|RND\\?|ANI\\?|AOT\\?|AIT\\?|AID\\?|TID\\?|TRD\\?|TOD\\?|TRO\\?|OAD\\?|IAD\\?|IOD\\?|TND\\?|OIR\\?|OID\\?', x=bruteforstlist2))
        #(!grepl(pattern = 'TTD\\?', x=bruteforstlist2))
        #grepl(pattern = 'AND', x=bruteforstlist2)

#hits=grepl(pattern = '^.ET', x=bruteforstlist2)&
#    (!grepl(pattern = 'RNT\\?|RND\\?|ANI\\?|AOT\\?|AIT\\?|AID\\?|TID\\?|TRD\\?|TOD\\?|TRO\\?|OAD\\?|IAD\\?|IOD\\?|TND\\?|OIR\\?|OID\\?', x=bruteforstlist2))

#hits=#grepl(pattern = '^\\.E', x=bruteforstlist2)&
#    (!grepl(pattern = 'RNT\\?|RND\\?|ANI\\?|AOT\\?|AIT\\?|AID\\?|TID\\?|TRD\\?|TOD\\?|TRO\\?|OAD\\?|IAD\\?|IOD\\?|TND\\?|OIR\\?|OID\\?|DR\\?', x=bruteforstlist2))

sum(    hits)
#lapply(bruteforstlist2[hits], function(S){substring(S, 1, 73)})
as.list(bruteforstlist2[hits])

current_options = which(hits)
names(current_options)=1:(sum(hits))
current_options
key_list[[1]]

#  DGTHKKPTMHSGGJVUURTCGDGTHKK
# _DKGHMJLZMHDGTEUR
extra_key=c('D'='H','H'='L','R'='R','P'='P','S'='S',
            'Q'='Q','U'='U','V'='V','W'='W','X'='X','Y'='Y','Z'='Z',
            'B'='I', 'O'='F')
R2=
lapply(key_list[hits], function(K) {
    mytmp=c(K, extra_key)[myInput2]
    mytmp[is.na(mytmp)]='.'
    paste0(mytmp,collapse='')})
R2

idx=which(grepl('_HOE',R2)&grepl('^HETLOO',R2))
key_list[idx]
idx
R2[idx]

HETKOO.TAK.EEN....T.EHETKOO.T..TDEHAND_HOEKAN..AKHETD..EN.OO.DAT...E..AND?...ADOOD.AN.E.KAN.EN.AA.KEE.E..ANO._....KDATHETE.ND..T.AA.N.ETDATHET.TO.T"
DGTHKKPTMHSGGJVUURTCGDGTHKKPTUBTEGDMJE_DKGHMJLZMHDGTEURGJVKKREMTBFIGARMJE?BFLMEKKEVMJVGRHMJLGJIMMRHGGOGRVMJKP_BFWBHEMTDGTGBJEBLTIMMRJBGTEMTDGTSTKPT


