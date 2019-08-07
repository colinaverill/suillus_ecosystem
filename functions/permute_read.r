dna.seq <- 'GTGYCAGCMGCCGCGGTAA'
#' premute_read.r
#' Returns all possible permutations of a DNA sequence with ambiguous bases.
#'
#' @param dna.seq a DNA sequence as a character vector.
#'
#' @return all possible permutations of a DNA sequence with ambiguous bases as a string of character entries.
#' @export
#'
#' @examples
#' permute_read('CCGYCAATTYMTTTRAGTTT')
permute_read <- function(dna.seq){
  l = tstrsplit(dna.seq, '', fixed = TRUE)
  #l = t(strsplit(dna.seq, '', fixed = TRUE)) #potential base method.
  all_bases = c('A', 'T', 'C', 'G')
  amino = c('A','C')
  purine = c('A','G')
  pyrmidine = c('C','T')
  
  #check for aminos.
  l = lapply(l, function(x) if (x == 'M') amino     else x)
  l = suppressWarnings(lapply(l, function(x) if (x == 'R') purine    else x))
  l = suppressWarnings(lapply(l, function(x) if (x == 'Y') pyrmidine else x))
  l = suppressWarnings(lapply(l, function(x) if (x == 'N') all_bases else x))
  
  #make potential primer vector.
  output <- Reduce(paste0, do.call(expand.grid, l))
  
  #return output.
  return(output)
}