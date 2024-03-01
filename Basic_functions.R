library(stringr)

signif_with_tailing_zeros <- function(number,digits,...){
	rounded <- as.character(signif(number, digits = digits)) 
	point_index = regexpr("\\.", rounded)
	if (point_index == -1){  # no change to scientific notation
		non_zeros = length(strsplit(rounded,'')[[1]])
	}
	else{
		two_parts = str_split(rounded,'\\.')[[1]]
		if (two_parts[1] == '0'){
			non_zeros = length(strsplit(two_parts[2],'')[[1]]) - regexpr("[1-9]+", two_parts[2]) + 1
		}
		else{
			non_zeros = length(strsplit(rounded,'')[[1]]) - 1
		}
	}
	if (non_zeros < digits){
		n_zeros = digits - non_zeros
		point = ifelse (grepl('.', rounded, fixed = TRUE), '', '.')
		paste0(as.character(rounded),point, paste(rep('0',n_zeros),collapse=''))
	}
	else{
		rounded
	}
}


########### in and out for R script
#### input using getopt()
# library(getopt)

# spec <-matrix(c(
#     "first",'f',2,"integer","This is first",		     #short name for the flag can be only length 1, which means limitation of number of parameters. 
#     "second",'s',1,"character",'this is second',  	     # 1 means 1 parameter value must be given after the flag. 
#     "third","t",2,'double','this is third',		     # 2 menas parameter value is not necessarily needed.
#     "four","o",0,'','this if four'                         # 0 means no parameter value but only the flag. Type can only be logical even you assign it numeric. No need to write it. 
# ),byrow=T,ncol=5)

## output log like plink
print_log <- function(logfile=''){
    args = commandArgs(trailingOnly = F)
    args[4] =  strsplit(args[4],'=' )[[1]][2]
    if(logfile == ''){
        logfile = paste0(args[4],'.log')    
    }
    cat(args[c(1,4,6:length(args))],file=logfile)
}
