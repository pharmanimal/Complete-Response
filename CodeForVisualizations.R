# Filename changed in R for easier handling
FDACRLs <-  CRLs_NonAppr_FDARelease_04Sept2025

# Plot: Location of applicant headquarters
ggplot(FDACRLs, aes(Headquarters)) + geom_bar() + coord_flip()+
  labs(
    title = "FDA Complete Response Letters",
    subtitle = "12 January 2024 – 18 August 2025",
    x = "# Pages in Letter", y = "Count")


# Plot: Therapeutic Areas represented in the letters

# 1. Piechart

namepie <- names(table(FDACRLs$TherapeuticArea))
pie(table(FDACRLs$TherapeuticArea),
    labels=1:length(namepie), 
    col = c("skyblue3","lightcyan1"), cex=0.6, 
    main = "FDA Complete Response Letters")

pieindex <- 1:length(namepie)
namepie <- paste(pieindex, namepie)
legend(x = 1.2, y = 1, namepie, cex = 0.6, title="Therapeutic Area")

# 2. Barplot

ggplot(FDACRLs, aes(TherapeuticArea))+ geom_bar()+coord_flip()+
  labs(
    title = "FDA Complete Response Letters",
    subtitle = "12 January 2024 – 18 August 2025")


# Plot: Issues in approval by discipline (Clin Pharm Earns an A-!)
IssueInApproval <- rbind(
  transform(table(FDACRLs$Regulatory)),
  transform(table(FDACRLs$ControlledSubstance)),
  transform(table(FDACRLs$Device)),
  transform(table(FDACRLs$HumanFactors)),
  transform(table(FDACRLs$Biopharm)),
  transform(table(FDACRLs$Statistics)),
  transform(table(FDACRLs$ClinicalPharmacology)),
  transform(table(FDACRLs$Nonclinical)),
  transform(table(FDACRLs$Clinical)),
  transform(table(FDACRLs$Quality)),
  transform(table(FDACRLs$Facility)))

IssueInApproval <- data.frame(
  Category = c(	rep("Regulatory", 2),
                rep("ControlledSubstance", 2),
                rep("Device", 2),
                rep("HumanFactors", 2),
                rep("Biopharm", 2),
                rep("Statistics", 2),
                rep("ClinicalPharmacology", 2),
                rep("Nonclinical", 2),
                rep("Clinical", 2),
                rep("Quality", 2),
                rep("Facility", 2)),
  ApprovalIssue = IssueInApproval[,1],
  Count = IssueInApproval[,2])

IssueInApproval$Category <- factor(IssueInApproval$Category, levels = c("Regulatory", "ControlledSubstance", "Device",
                                                                        "HumanFactors", "Biopharm",
                                                                        "Statistics", "ClinicalPharmacology",
                                                                        "Nonclinical", "Clinical", "Quality", "Facility"))

ggplot(IssueInApproval, aes(x = Category, y = Count, fill = ApprovalIssue)) +
  geom_bar(stat = "identity", position = "stack") + coord_flip() +
  scale_fill_manual(values=c('gray ', 'blue '))+ 
  labs(title = "Approvability Issue in Complete Response Letter: 1/12/2024 – 8/18/2025",
       x = " ",
       y = "Count") +
  theme(legend.position="none")


# Plot: Number of pages in complete response letters
c <- ggplot(FDACRLs, aes(PagesInLetter))
# c + geom_histogram(binwidth = 2, fill="steelblue", ) +
c + geom_histogram(binwidth = 2, color = "#000000", fill = "#0099F8") +
  labs(
    title = "FDA Complete Response Letters",
    subtitle = "12 January 2024 – 18 August 2025",
    x = "# Pages in Letter", y = "Count") + 
  theme_classic()
