
from rocker/shiny-verse:4.4.1
run apt-get update && apt-get install -y libxml2-dev libglpk-dev unixodbc git p7zip-full    
                                                                               
run R -e "install.packages(c('dplyr', 'reshape2', 'RColorBrewer', 'pROC', 'text2vec', 'flexdashboard', 'tidypmc', 'tm', 'R.utils', 'igraph', 'europepmc'))"
                                                                                
run R -e "remotes::install_git('https://gitlab.com/thomaschln/opticskxi.git', dependencies = TRUE)"  
run R -e "install.packages('kgraph')"
run R -e "remotes::install_git('https://gitlab.com/thomaschln/nlpembeds.git')"  

run wget -O /epmc_1700_suic_db.xml.7z "https://www.dropbox.com/scl/fi/49jjes22ldssydzechdch/epmc_1700_suic_db.xml.7z?rlkey=a8j0aj0ov1wxj9c9qjobzof53&st=0qstlgya&dl=0"
                                                                                
run mkdir /root/.ssh
run ssh-keyscan gitlab.com >> /root/.ssh/known_hosts
run git clone https://gitlab.com/thomaschln/psychclust_rmed24.git
                                                    
run 7z x /epmc_1700_suic_db.xml.7z -o/psychclust_rmed24/inst/extdata            

run R -e "remotes::install_git('https://gitlab.com/thomaschln/ehrdb.git', ref = 'e8637c20676b6dc71711d0c6f43cbfc138480253')"  

# from https://github.com/ccb-hms/HarvardInovalonUserGuide

# this package allows R to make calls through unixODBC
run R -e "install.packages('odbc')"

# this package makes establishing a database connection easy by generating connection strings
run R -e "remotes::install_git('https://github.com/nathan-palmer/MsSqlTools.git', ref='v1.1.0')"

# this package provides a performant pivot function to reshape a fact table to a 
# design matrix
run R -e "remotes::install_git('https://github.com/nathan-palmer/FactToCube.git', ref='v1.0.0')"

# this package contains several convenience functions
run R -e "remotes::install_git('https://github.com/nathan-palmer/SqlTools.git', ref='v1.0.0')"
                                                                                
run cd /psychclust_rmed24 && make roxygenise                                    
run R -e "devtools::install('psychclust_rmed24', dependencies = TRUE)"          

