#### compiling ####
setwd("manuscrit")
Sweave("ICU_Report_Guerra_Jaunatre.Rnw")
Stangle("ICU_Report_Guerra_Jaunatre.Rnw")
tools::texi2pdf("ICU_Report_Guerra_Jaunatre.tex") 
file.remove(list.files()[ which(! list.files() %in% c("child_test.Rnw", 
                                                      "fig","ICU.bib", 
                                                      "ICU_Report_Guerra_Jaunatre.Rnw", 
                                                      "ICU_Report_Guerra_Jaunatre.pdf",
                                                      "ICU_Report_Guerra_Jaunatre_oldversion.Rnw") )]
)
setwd("../")

# system('xdg-open manuscrit/ICU_Report_Guerra_Jaunatre.pdf ') # works on ubuntu
