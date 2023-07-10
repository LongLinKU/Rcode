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
