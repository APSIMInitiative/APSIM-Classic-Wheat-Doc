# * Author:    Bangyou Zheng (Bangyou.Zheng@csiro.au)
# * Created:   11:29 AM Thursday, 20 March 2014
# * Copyright: AS IS
# *


# Convert wheat documentation to html
rm(list = ls())
doc <- readLines('WheatDocumentation.lyx')


pos <- grep('dev = \'pdf\'', doc)
doc[pos] <- gsub('dev = \'pdf\'', 'dev = \'png\'', doc[pos])

pos <- grep('\\.pdf', doc)
doc[pos] <- gsub('\\.pdf', '', doc[pos])

pos <- grep('source\\(\'Rcode/wdVisXY.R\'\\)', doc)
doc[pos] <- gsub('source\\(\'Rcode/wdVisXY.R\'\\)', 
    'source\\(\'../Rcode/wdVisXY.R\'\\)', doc[pos])
    
pos <- grep('source\\(\'Rcode/wdFunctions.R\'\\)', doc)
doc[pos] <- gsub('source\\(\'Rcode/wdFunctions.R\'\\)', 
    'source\\(\'../Rcode/wdFunctions.R\'\\)', doc[pos])

pos <- grep('xmlInternalTreeParse\\(\'wheat.xml\'\\)', doc)
doc[pos] <- gsub('xmlInternalTreeParse\\(\'wheat.xml\'\\)', 
    'xmlInternalTreeParse\\(\'../wheat.xml\'\\)', doc[pos])

figures <- c('wdBiomassPartitioning.png', 'wdWheatPhenology.png',
    'wdBiomassPartition.png')

    
dir.create('html')
setwd('html')

dir.create('figure')
file.copy(paste0('../figure/', figures), 'figure/')
file.copy('../tex4ht.cfg', '.')

writeLines(doc, 'wheat.lyx')

system('lyx -f --export "latex" wheat.lyx')

# Change the path of bibliography
tex <- readLines('wheat.tex')
pos <- grep('\\\\bibliographystyle', tex)
tex[pos] <- gsub('(\\\\bibliographystyle\\{).*(\\})', 
    '\\1../elsart-harv\\2', tex[pos])
pos <- grep('\\\\bibliography\\{', tex)
tex[pos] <- gsub('(.*\\\\bibliography\\{).*(\\})', 
    '\\1../wd\\2', tex[pos])
pos <- grep('type=eps', tex)
tex[pos] <- gsub(', type=eps', '', tex[pos])
pos <- grep('.*\\\\includegraphics.*\\{.*\\}.*', tex)
tex[pos] <- gsub('(.*\\\\includegraphics.*\\{)(.*/.*)(\\}.*)',
    '\\1\\2.png\\3', tex[pos])

pos <- grep('wdBiomassPartitioning.png', tex)

tex[pos] <- gsub('(.*\\\\includegraphics.*\\{)(.*)(\\}.*)',
    '\\1\\figure/wdBiomassPartitioning.png\\3', tex[pos])
    
writeLines(tex, 'wheat.tex')

# Convert to html
system('latex wheat')
system('bibtex wheat')
system('latex wheat')
system('latex wheat')
system('htlatex wheat "tex4ht,html,word"')

# Change the png path
pngs <- list.files('.', 'wheat\\d+x.png')
dir.create('equation')
file.copy(pngs, 'equation')
file.remove(pngs)
html <- readLines('wheat.html')
pos <- grep('(^.*src=")(wheat\\d+x\\.png)(".*$)', html)
html[pos] <- gsub('(^.*src=")(wheat\\d+x\\.png)(".*$)',
    '\\1equation/\\2\\3', html[pos])
pos <- grep('^<img', html)
html[pos] <- paste0('<div align="middle">', html[pos])

for (i in seq(along = pos))
{
    pos_i <- grep('>', html[seq(pos[i] + 1, pos[i] + 10)])[1] + pos[i]
    pos_2 <- as.numeric(regexpr('>', html[pos_i]))
    html[pos_i] <- paste0(substring(html[pos_i], 1, pos_2),
        '</div>',
        substring(html[pos_i], pos_2 + 1, nchar(html[pos_i])))    
}


writeLines(html, 'wheat.html')

files <- list.files('.', '\\.', full.names = TRUE)
file.remove(files[!(files %in% 
    list.files('.', 'html|css', full.names = TRUE))])

setwd('..')
