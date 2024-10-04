
from rocker/shiny-verse:4.4.1
run apt-get update && \
  # for vignette and checks
  apt-get install -y --no-install-recommends texlive texlive-latex-recommended texlive-fonts-extra qpdf tidy && \
  # for igraph
  apt-get install -y libxml2-dev libglpk-dev

                                                                               
run R -e "install.packages(c('dplyr', 'reshape2', 'RColorBrewer', 'pROC', 'text2vec', 'flexdashboard', 'tidypmc', 'tm', 'R.utils', 'igraph', 'europepmc'))"
                                                                                
run apt-get update && apt-get install -y git                                    
                                                                                
run R -e "remotes::install_git('https://gitlab.com/thomaschln/opticskxi.git', dependencies = TRUE)"  
run R -e "remotes::install_git('https://gitlab.com/thomaschln/sgraph.git')"     
run R -e "remotes::install_git('https://gitlab.com/thomaschln/kgraph.git')"     
run R -e "remotes::install_git('https://gitlab.com/thomaschln/nlpembeds.git')"  

run wget -O /epmc_1700_suic_db.xml.7z "https://www.dropbox.com/scl/fi/49jjes22ldssydzechdch/epmc_1700_suic_db.xml.7z?rlkey=a8j0aj0ov1wxj9c9qjobzof53&st=0qstlgya&dl=0"
                                                                                
run apt-get update && apt-get install -y p7zip-full    

run mkdir /root/.ssh
run ssh-keyscan gitlab.com >> /root/.ssh/known_hosts
run git clone https://gitlab.com/thomaschln/psychclust_rmed24.git
                                                    
run 7z x /epmc_1700_suic_db.xml.7z -o/psychclust_rmed24/inst/extdata            

run R -e "remotes::install_git('https://gitlab.com/thomaschln/ehrdb.git')"  
                                                                                
run cd /psychclust_rmed24 && make roxygenise                                    
run R -e "devtools::install('psychclust_rmed24', dependencies = TRUE)"          

