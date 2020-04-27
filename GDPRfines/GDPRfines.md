GDPR Fines
================
Jim Gruman
27 April 2020

This week’s [`#TidyTuesday`
dataset](https://github.com/rfordatascience/tidytuesday) is on EU GDPR
violations.

In addition, R version 4.0.0 **Arbor Day** was just released. I am
re-installing packages as-required while going through projects like
this one.

The R Studio team recently launched
[`tidymodels.org`](https://www.tidymodels.org/), a new central location
with resources and documentation for tidymodels packages. Check out the
[official blog
post](https://www.tidyverse.org/blog/2020/04/tidymodels-org/) for more
details.

Julia Silge published a great blog post with [another screencast
demonstrating how to use
`tidymodels`](https://juliasilge.com/category/tidymodels/). She includes
a good video for folks getting started with `tidymodels`.

## Explore the data

Our modeling goal here is to understand what kind of GDPR violations are
associated with higher fines in the [\#TidyTuesday
dataset](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-21/readme.md)
for this week. Before we start, what are the most common GDPR articles
actually about? Roughly speaking:

  - **Article 5:** principles for processing personal data (legitimate
    purpose, limited)
  - **Article 6:** lawful processing of personal data (i.e. consent,
    etc)
  - **Article 13:** inform subject when personal data is collected
  - **Article 15:** right of access by data subject
  - **Article 32:** security of processing (i.e. data breaches)

Let’s get started by looking at the data on violations.

``` r
gdpr_raw <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv")

gdpr_raw %>%
  knitr::kable("html") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = F, fixed_thead = T
  ) %>%
  kableExtra::scroll_box(width = "800px", height = "200px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:800px; ">

<table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

id

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

picture

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

name

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

price

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

authority

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

date

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

controller

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

article\_violated

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

type

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

source

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

summary

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/republic-of-poland.svg>

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

9380

</td>

<td style="text-align:left;">

Polish National Personal Data Protection Office (UODO)

</td>

<td style="text-align:left;">

10/18/2019

</td>

<td style="text-align:left;">

Polish Mayor

</td>

<td style="text-align:left;">

Art. 28 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://uodo.gov.pl/decyzje/ZSPU.421.3.2019>

</td>

<td style="text-align:left;">

No data processing agreement has been concluded with the company whose
servers contained the resources of the Public Information Bulletin (BIP)
of the Municipal Office in Aleksandrów Kujawski. For this reason, a fine
of 40.000 PLN (9400 EUR) was imposed on the mayor of the city.

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

10/17/2019

</td>

<td style="text-align:left;">

UTTIS INDUSTRIES

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 13 GDPR|Art. 5 (1) c) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=A_patra_amenda&lang=ro>

</td>

<td style="text-align:left;">

A controller was sanctioned because he had unlawfully processed the
personal data (CNP), and images of employees obtained through the
surveillance system. The disclosure of the CNP in a report for the ISCIR
training in 2018 wasn’t legal, as per Art.6 GDPR.

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

10/16/2019

</td>

<td style="text-align:left;">

Xfera Moviles S.A.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00262-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The company had unlawfully processed the personal data despite the
subject’s request to stop doing so.

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

8000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

10/16/2019

</td>

<td style="text-align:left;">

Iberdrola Clientes

</td>

<td style="text-align:left;">

Art. 31 GDPR

</td>

<td style="text-align:left;">

Failure to cooperate with supervisory authority

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00304-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Iberdrola Clientes violated Article 13 of the GDPR when it showed a
complete lack of cooperation with the AEPD. The latter had requested
Iberdrola Clientes to provide the necessary information needed to add a
person to the solvency list.

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

10/09/2019

</td>

<td style="text-align:left;">

Raiffeisen Bank SA

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Comunicat_Presa_09_10_2019&lang=ro>

</td>

<td style="text-align:left;">

Raiffeisen Bank Romania did not observe the necessary security measures
required by the GDPR when it assessed the scores of individuals on the
WhatsApp platform. The personal data was exchanged via WhatsApp.

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

10/09/2019

</td>

<td style="text-align:left;">

Vreau Credit SRL

</td>

<td style="text-align:left;">

Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Comunicat_Presa_09_10_2019&lang=ro>

</td>

<td style="text-align:left;">

The Company sent personal information through the WhatsApp platform to
Raiffeisen Bank in order to facilitate the assessment of personal
scores. The results were returned on the same platform.

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/greece.svg>

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

200000

</td>

<td style="text-align:left;">

Hellenic Data Protection Authority (HDPA)

</td>

<td style="text-align:left;">

10/07/2019

</td>

<td style="text-align:left;">

Telecommunication Service Provider

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 25 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.dpa.gr/APDPXPortlets/htdocs/documentSDisplay.jsp?docid=3,241,32,146,79,143,149,112>

</td>

<td style="text-align:left;">

Despite the clear refusal of telemarketing calls by the customers, the
company proceeded to ignore this because of technical errors.

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/greece.svg>

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

200000

</td>

<td style="text-align:left;">

Hellenic Data Protection Authority (HDPA)

</td>

<td style="text-align:left;">

10/07/2019

</td>

<td style="text-align:left;">

Telecommunication Service Provider

</td>

<td style="text-align:left;">

Art. 21 (3) GDPR|Art. 25 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.dpa.gr/APDPXPortlets/htdocs/documentSDisplay.jsp?docid=3,241,32,146,79,143,149,112>

</td>

<td style="text-align:left;">

Due to technical errors, the personal data of 8.000 customers
wasn&\#8217;t deleted upon request

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

10/01/2019

</td>

<td style="text-align:left;">

Vueling Airlines

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00300-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Vueling Airlines made it impossible for users to access their website
without accepting the cookies. Therefore, one couldn’t browse the
website unless they accepted the cookies. The AEPD sanctioned the
company with 30.000 euros

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

09/26/2019

</td>

<td style="text-align:left;">

Inteligo Media SA

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 (1) a) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Alta_sanctiune_RGPD&lang=ro>

</td>

<td style="text-align:left;">

An operator utilized an unfilled checkbox through which users could
request that they do not receive any emails from the company. Since they
couldn’t do that, they continued receiving information via email.

</td>

</tr>

<tr>

<td style="text-align:right;">

11

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

195407

</td>

<td style="text-align:left;">

Data Protection Authority of Berlin

</td>

<td style="text-align:left;">

09/19/2019

</td>

<td style="text-align:left;">

Delivery Hero

</td>

<td style="text-align:left;">

Art. 15 GDPR|Art. 17 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.datenschutz-berlin.de/fileadmin/user_upload/pdf/pressemitteilungen/2019/20190919-PM-Bussgelder.pdf>

</td>

<td style="text-align:left;">

The Company had retained the personal data of customers who had
expressed their desire to discontinue receiving emails from the company.
Eight customers complained to have received such emails, despite not
having solicited them. Moreover, the company refused to share
information with five subjects regarding their rights to withdraw
consent in the processing of personal information.

</td>

</tr>

<tr>

<td style="text-align:right;">

12

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/belgium.svg>

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Belgian Data Protection Authority (APD)

</td>

<td style="text-align:left;">

09/19/2019

</td>

<td style="text-align:left;">

Merchant

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.sudinfo.be/id141981/article/2019-09-19/un-commercant-recu-une-amende-de-10000-euros-pour-avoir-voulu-creer-une-carte-de>

</td>

<td style="text-align:left;">

A merchant was found guilty of trying to create a customer card using an
electronic identity card. In doing so, the merchant would have needed
access to personal information on the electronic identity card,
including photo and barcode. The fine was 10.000 euros.

</td>

</tr>

<tr>

<td style="text-align:right;">

13

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/republic-of-poland.svg>

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

644780

</td>

<td style="text-align:left;">

Polish National Personal Data Protection Office (UODO)

</td>

<td style="text-align:left;">

09/10/2019

</td>

<td style="text-align:left;">

Morele.net

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://uodo.gov.pl/decyzje/ZSPR.421.2.2019>

</td>

<td style="text-align:left;">

Morele.net was sanctioned with a fine of PLN 2.8 million because it
hadn’t ensured the proper security standards of customers’ data. As a
consequence, more than 2.2 million people had their personal data
accessed illegally.

</td>

</tr>

<tr>

<td style="text-align:right;">

14

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/austria.svg>

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Austrian Data Protection Authority (DSB)

</td>

<td style="text-align:left;">

08/30/2019

</td>

<td style="text-align:left;">

Medical Company

</td>

<td style="text-align:left;">

Art. 13 GDPR|Art. 37 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://apps.derstandard.at/privacywall/story/2000107377808/fussballerinnen-nackt-gefilmt-mostviertler-trainer-muss-strafe-zahlen>

</td>

<td style="text-align:left;">

The company was fined because it had refused to comply with the
obligation of appointing a data protection officer.

</td>

</tr>

<tr>

<td style="text-align:right;">

15

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2600000

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

08/28/2019

</td>

<td style="text-align:left;">

National Revenue Agency

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=news_view&aid=1519>

</td>

<td style="text-align:left;">

Because of the inappropriate handling of personal data, more than 6
million individuals had their data hacked. This informational leak was a
direct cause of the company&\#8217;s security laxity.

</td>

</tr>

<tr>

<td style="text-align:right;">

16

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

511000

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

08/28/2019

</td>

<td style="text-align:left;">

DSK Bank

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dvi.gov.lv/lv/zinas/datu-valsts-inspekcija-piemero-7000-eiro-lielu-naudas-sodu-internetveikalam-par-personas-datu-apstrades-parkapumiem/>

</td>

<td style="text-align:left;">

Data leakage due to the inappropriate security and organizational
measures of the company. Information related to more than 23.000 credits
records belonging to more than 33.000 customers were made public. The
data included names, ID numbers, biometric data, addresses, and copies
of identity cards.

</td>

</tr>

<tr>

<td style="text-align:right;">

17

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/latvia.svg>

</td>

<td style="text-align:left;">

Latvia

</td>

<td style="text-align:right;">

7000

</td>

<td style="text-align:left;">

Data State Inspectorate (DSI)

</td>

<td style="text-align:left;">

08/26/2019

</td>

<td style="text-align:left;">

Online services provider

</td>

<td style="text-align:left;">

Art. 17 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.dvi.gov.lv/lv/zinas/datu-valsts-inspekcija-piemero-7000-eiro-lielu-naudas-sodu-internetveikalam-par-personas-datu-apstrades-parkapumiem/>

</td>

<td style="text-align:left;">

The merchant had ignored a client’s demands to stop processing personal
data, in particular, the phone number. The merchant had continued
sending the subject advertising messages to the subject&\#8217;s phone
number.

</td>

</tr>

<tr>

<td style="text-align:right;">

18

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/sweden.svg>

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:right;">

18630

</td>

<td style="text-align:left;">

Data Protection Authority of Sweden

</td>

<td style="text-align:left;">

08/20/2019

</td>

<td style="text-align:left;">

Skellefteå school

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 9 GDPR|Art. 35 GDPR|Art. 36 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.datainspektionen.se/globalassets/dokument/beslut/facial-recognition-used-to-monitor-the-attendance-of-students.pdf>

</td>

<td style="text-align:left;">

A school attempted to introduce the use of facial recognition software
to facilitate the attendance process of students. The school was
ultimately fined because the means used to monitor attendance were
disproportionate to the goal itself. Moreover, students and their
parents couldn’t freely withdraw consent from being monitored to
validate attendance. Furthermore, one case of processing activity
presented elevated risks since it involved children dependent on the
high-school board. Ultimately, the school didn’t observe Art. 35 of the
GDPR.

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

08/16/2019

</td>

<td style="text-align:left;">

Avon Cosmetics

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00159-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A client had complained that AVOND COSMETICS hadn’t observed the law
when it processed his personal data erroneously. His identity wasn’t
properly verified, which led to the erroneous matching of that client
with a register of claims. As a result, the client wasn’t able to work
with his bank. Moreover, a third-party utilized the client’s personal
data unlawfully.

</td>

</tr>

<tr>

<td style="text-align:right;">

20

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/austria.svg>

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

11000

</td>

<td style="text-align:left;">

Austrian Data Protection Authority (DSB)

</td>

<td style="text-align:left;">

07/31/2019

</td>

<td style="text-align:left;">

Private individual (football coach)

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://apps.derstandard.at/privacywall/story/2000107377808/fussballerinnen-nackt-gefilmt-mostviertler-trainer-muss-strafe-zahlen>

</td>

<td style="text-align:left;">

A soccer coach was fined for having covertly filmed female players while
they were taking showers. This had taken place for many years.

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/greece.svg>

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Hellenic Data Protection Authority (HDPA)

</td>

<td style="text-align:left;">

07/30/2019

</td>

<td style="text-align:left;">

PWC Business Solutions

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 5 (2) GDPR|Art. 6 (1) GDPR|Art. 13 (1) c) GDPR|Art.
14 (1) c) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dpa.gr/pls/portal/docs/PAGE/APDPX/ENGLISH_INDEX/DECISIONS/SUMMARY%20OF%20DECISION%2026_2019%20(EN).PDF>

</td>

<td style="text-align:left;">

The company unlawfully processed the employer&\#8217;s data while
creating the illusion that it acted under the legal basis of consent.
Whereas, the company was using a different legal basis. This is a strict
violation of the transparency principle. Moreover, the company violated
the accountability principle when it failed to bring evidence related to
the proper assessment of the employer&\#8217;s data using the right
legal bases.

</td>

</tr>

<tr>

<td style="text-align:right;">

22

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/france.svg>

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

180000

</td>

<td style="text-align:left;">

French Data Protection Authority (CNIL)

</td>

<td style="text-align:left;">

07/25/2019

</td>

<td style="text-align:left;">

Active Assurances

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.legifrance.gouv.fr/affichCnil.do?id=CNILTEXT000038810992>

</td>

<td style="text-align:left;">

The company had allowed for personal data belonging to clients
(including copies of the driver’s license) to be publicized online.
Apparently, unauthorized access was detected, and the fault lies with
the inappropriate security measures.

</td>

</tr>

<tr>

<td style="text-align:right;">

23

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/united-kingdom.svg>

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Information Commissioner (ICO)

</td>

<td style="text-align:left;">

07/09/2019

</td>

<td style="text-align:left;">

Marriott International, Inc

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://ico.org.uk/about-the-ico/news-and-events/news-and-blogs/2019/07/statement-intention-to-fine-marriott-international-inc-more-than-99-million-under-gdpr-for-data-breach/>

</td>

<td style="text-align:left;">

While the trial hasn’t ended yet, the ICO intends to fine the company.
This is in accordance with Art. 32 of the GDPR, which the company
allegedly infringed in a cyber-incident in November 2018. The incident
involved the public exposal of personal records belonging to over 339
million people, out of which 31 million were residents of the European
Economic Area. This vulnerability is believed to have been present in
the Starwood hotels group, which Marriott International acquired. Due to
the inappropriate and insufficient attention paid to the security of the
systems, the ICO believes a fine is in
order.\<strong\>Notice:\</strong\> Marriott is facing a fine of
€110,390,200, but this is not yet final. As such, it’s not included in
our statistics dashboard.

</td>

</tr>

<tr>

<td style="text-align:right;">

24

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/united-kingdom.svg>

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Information Commissioner (ICO)

</td>

<td style="text-align:left;">

07/08/2019

</td>

<td style="text-align:left;">

British Airways

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://ico.org.uk/about-the-ico/news-and-events/news-and-blogs/2019/07/ico-announces-intention-to-fine-british-airways/>

</td>

<td style="text-align:left;">

The ICO notified the British Airways of its intention to issue a fine
worth 183.39 million pounds because of an alleged infringement of Art.
31 of the GDPR. The reason for this is related to an incident which the
company reported in September 2018, when the British Airways website had
diverted the users’ traffic to a dangerous website. The hackers in
charge of this website had stolen the personal data of more than 500.000
customers. The company had poor security mechanisms to prevent such
cyber-attacks from happening.\<strong\>Notice:\</strong\> British
Airways is facing a fine of €204,600,000, but this is not yet final. As
such, it’s not included in our statistics dashboard.

</td>

</tr>

<tr>

<td style="text-align:right;">

25

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

07/05/2019

</td>

<td style="text-align:left;">

Legal Company & Tax Hub SRL

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=2019%20A%20treia%20amenda%20in%20aplicarea%20RGPD&lang=ro>

</td>

<td style="text-align:left;">

The company had not imposed sufficient security measures, which led to
the unauthorized access of personal information related to the people
who had made transactions with the website avocatoo.ro. This information
includes names, emails, phone numbers, jobs, surnames, mailing
addresses, and transaction details). Documents dated 10th of November
2018 – 1st of February 2019 had become publicly accessible to anyone.
The company was sanctioned following a notification by the National
Supervisory Authority when transaction details were publicly accessible
via two links.

</td>

</tr>

<tr>

<td style="text-align:right;">

26

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

07/02/2019

</td>

<td style="text-align:left;">

World Trade Center Bucharest SA

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=O_noua_amenda_GDPR&lang=ro>

</td>

<td style="text-align:left;">

A printed checklist used to verify the attendance of breakfast customers
(approx. 46 clients) was photographed by unauthorized people. As a
result, the personal data of those clients was disclosed to the public.
The operator working for the hotel was sanctioned because of
insufficient security measures.

</td>

</tr>

<tr>

<td style="text-align:right;">

27

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

130000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

06/27/2019

</td>

<td style="text-align:left;">

Unicredit Bank SA

</td>

<td style="text-align:left;">

Art. 25 (1) GDPR|Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Comunicat_Amenda_Unicredit&lang=ro>

</td>

<td style="text-align:left;">

The Company was issued a fine because it had failed to provide the
necessary security and organization measures in two cases. Firstly, it
failed in the appropriate determination of the data processing means.
Secondly, it failed in the appropriate implementation of necessary
security safeguards, which led to the public disclosure of the personal
data of over 337.042 people.

</td>

</tr>

<tr>

<td style="text-align:right;">

28

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

15150

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

06/25/2019

</td>

<td style="text-align:left;">

Budapest Police Command

</td>

<td style="text-align:left;">

Art. 33 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-2471-hatarozat.pdf>

</td>

<td style="text-align:left;">

The data controllers didn’t exert sufficient rigorousness when handling
personal client data, which led to the displacement of a flash memory
stick with personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

29

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/netherlands.svg>

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

460000

</td>

<td style="text-align:left;">

Dutch Supervisory Authority for Data Protection (AP)

</td>

<td style="text-align:left;">

06/18/2019

</td>

<td style="text-align:left;">

Hague Hospital

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://autoriteitpersoonsgegevens.nl/nl/nieuws/haga-beboet-voor-onvoldoende-interne-beveiliging-pati%C3%ABntendossiers>

</td>

<td style="text-align:left;">

After a serious investigation, the DDPA surmised that the Hague Hospital
failed to provide the appropriate security measures for possession of
patient records. This investigation had started following several events
when multiple staff hospital members had checked the personal data of a
Dutch person. Measures were taken, and the hospital was warned – it
would have to update its security measures by the 2nd of October 2019 or
it would incur e penalty of 100.000 EUR every two weeks.

</td>

</tr>

<tr>

<td style="text-align:right;">

30

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/france.svg>

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

French Data Protection Authority (CNIL)

</td>

<td style="text-align:left;">

06/13/2019

</td>

<td style="text-align:left;">

Uniontrad Company

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 12 GDPR|Art. 13 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.legifrance.gouv.fr/affichCnil.do?oldAction=rechExpCnil&id=CNILTEXT000038629823&fastReqId=946473298&fastPos=1>

</td>

<td style="text-align:left;">

Complaints from the employees were received that they were unlawfully
filmed in the workspace. The company failed to observe the rules
pertaining to the unlawful filming of employees all the time, and the
necessity of providing information related to the data processing to the
employees. The CNIL performed an audit in October 2018, and the company
wasn’t observing the data protection laws. Therefore, fines were issued.

</td>

</tr>

<tr>

<td style="text-align:right;">

31

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

250000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

06/11/2019

</td>

<td style="text-align:left;">

Professional Football League (LaLiga)

</td>

<td style="text-align:left;">

Art. 5 (1) a)|Art. 7 (3) GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.eldiario.es/tecnologia/Agencia-Proteccion-Datos-Liga-microfono_0_908859408.html#click=https://t.co/RI3qZzucaB>

</td>

<td style="text-align:left;">

A fine was issued to the National Football League (LaLiga) because it
had failed to inform users of the implications contained within the app
it offered. This app remotely accessed the users&\#8217; microphones
once every minute to check pubs screening football matches. The AEPD
thinks that the users were not sufficiently informed of this. Moreover,
the users did not have the adequate possibility to withdraw their
consent, once given.

</td>

</tr>

<tr>

<td style="text-align:right;">

32

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/denmark.svg>

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

200850

</td>

<td style="text-align:left;">

Danish Data Protection Authority (Datatilsynet)

</td>

<td style="text-align:left;">

06/03/2019

</td>

<td style="text-align:left;">

IDdesign A/S

</td>

<td style="text-align:left;">

Art. 5 (1) e) GDPR|Art. 5 (2) GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.datatilsynet.dk/tilsyn-og-afgoerelser/afgoerelser/2019/jun/tilsyn-med-iddesigns-behandling-af-personoplysninger/>

</td>

<td style="text-align:left;">

After an inspection in 2018 when irregularities were noticed, the
company IDdesign was fined. The company had overused the data of over
380.000 customers for a longer period of time than they were allowed to,
as per the initial goals of the data processing. Moreover, the company
had no clear deadlines regarding the deletion of personal data. The
controller had also ignored the necessity of having a clear policy on
the data deletion procedures.

</td>

</tr>

<tr>

<td style="text-align:right;">

33

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/belgium.svg>

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Belgian Data Protection Authority (APD)

</td>

<td style="text-align:left;">

05/28/2019

</td>

<td style="text-align:left;">

A mayor

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Several

</td>

<td style="text-align:left;">

<https://www.autoriteprotectiondonnees.be/news/lautorite-de-protection-des-donnees-prononce-une-sanction-dans-le-cadre-dune-campagne>

</td>

<td style="text-align:left;">

A mayor was fined for having misused people’s personal data during a
political campaign.

</td>

</tr>

<tr>

<td style="text-align:right;">

34

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/france.svg>

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

400000

</td>

<td style="text-align:left;">

French Data Protection Authority (CNIL)

</td>

<td style="text-align:left;">

05/28/2019

</td>

<td style="text-align:left;">

SERGIC

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.legifrance.gouv.fr/affichCnil.do?oldAction=rechExpCnil&id=CNILTEXT000038552658&fastReqId=119744754&fastPos=1>

</td>

<td style="text-align:left;">

The company was fined because of two reasons – the complete lack of
security measures, and excessive data storage. Regarding the former
reason, personal data, including health cards, IDs, divorce judgments,
and account statements were available online with no authentication
procedure. Moreover, the company breached the data storage deadline it
had in place and kept clients&\#8217; data for more than it should have.

</td>

</tr>

<tr>

<td style="text-align:right;">

35

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

92146

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

05/23/2019

</td>

<td style="text-align:left;">

Organizer of SZIGET festival and VOLT festival

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) b) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing and information
obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-55_hatarozat.pdf>

</td>

<td style="text-align:left;">

The subjects had not been informed about the data processing, and the
data controllers had not complied with the principle of purpose
limitation.

</td>

</tr>

<tr>

<td style="text-align:right;">

36

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/lithuania.svg>

</td>

<td style="text-align:left;">

Lithuania

</td>

<td style="text-align:right;">

61500

</td>

<td style="text-align:left;">

Lithuanian Data Protection Authority (VDAI)

</td>

<td style="text-align:left;">

05/16/2019

</td>

<td style="text-align:left;">

UAB MisterTango

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security
and information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.ada.lt/go.php/lit/Imones-atsakomybes-neisvengs--lietuvoje-skirta-zenkli-bauda-uz-bendrojo-duomenu-apsaugos-reglamento-pazeidimus-/1>

</td>

<td style="text-align:left;">

The data controllers had overextended his authority to collect
unwarranted information about the clients. Moreover, a data breach took
place from 09-10 July 2018, when payment data was made available on the
internet. Moreover, the data controllers had not reported the data
breach.

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

3105

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

05/13/2019

</td>

<td style="text-align:left;">

Not known

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) b) GDPR|Art. 32 (1) GDPR

</td>

<td style="text-align:left;">

Not known

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34471>

</td>

<td style="text-align:left;">

Not available.

</td>

</tr>

<tr>

<td style="text-align:right;">

38

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1400

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

05/09/2019

</td>

<td style="text-align:left;">

Police Officer

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.baden-wuerttemberg.datenschutz.de/lfdi-baden-wuerttemberg-verhaengt-erstes-bussgeld-gegen-polizeibeamten/>

</td>

<td style="text-align:left;">

The police officer acted outside the boundaries of the law when he used
the Central Traffic Information System to find out the personal data of
the license plate of an unknown person. Moreover, he then proceeded with
a SARS inquiry, gathering personal data of the injured parties (mobile
and home phone numbers). The police officer then contacted the wounded
party. These actions were done outside his lawful prerogatives, and it
is an infringement of personal data. However, he acted not in trying to
exercise official duties but to satisfy personal inquiries. Therefore,
the police department is not to blame.

</td>

</tr>

<tr>

<td style="text-align:right;">

39

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

194

</td>

<td style="text-align:left;">

Czech Data Protection Auhtority (UOOU)

</td>

<td style="text-align:left;">

05/06/2019

</td>

<td style="text-align:left;">

Public utility company

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34472>

</td>

<td style="text-align:left;">

Not available.

</td>

</tr>

<tr>

<td style="text-align:right;">

40

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/norway.svg>

</td>

<td style="text-align:left;">

Norway

</td>

<td style="text-align:right;">

203000

</td>

<td style="text-align:left;">

Norwegian Supervisory Authority (Datatilsynet)

</td>

<td style="text-align:left;">

04/29/2019

</td>

<td style="text-align:left;">

Oslo Municipal Education Department

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.datatilsynet.no/contentassets/f7246f38ff394d32bef6895bc65a4b4f/varsel-om-gebyr---oslo-kommune.pdf>

</td>

<td style="text-align:left;">

The fine was issued on the following grounds: insufficient security
measures established on the app launched by an Oslo school. This app
allowed students and parents to contact teachers in real-time. However,
unauthorized access was detected, and unknown people gained access to
personal data related to students and school employees.

</td>

</tr>

<tr>

<td style="text-align:right;">

41

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/republic-of-poland.svg>

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

12950

</td>

<td style="text-align:left;">

Polish National Personal Data Protection Office (UODO)

</td>

<td style="text-align:left;">

04/25/2019

</td>

<td style="text-align:left;">

Sports association

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://uodo.gov.pl/pl/138/990>

</td>

<td style="text-align:left;">

The sports association published personal data related to judges who had
received judicial licenses online. Moreover, the exact addresses and
PESEL numbers of these judges became public. As the sports association
acted outside the law, fines were in order. However, there were
mitigating circumstances in that the sports association immediately
noticed its mistakes and attempted to remove the data from the public
domain. Still, these attempts were ineffective, and a penalty was
issued. The 585 judges had suffered no damage because of this, so the
penalty was adjusted by the president of the Office of Competition and
Consumer Protection.

</td>

</tr>

<tr>

<td style="text-align:right;">

42

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/italy.svg>

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Italian Data Protection Authority (Garante)

</td>

<td style="text-align:left;">

04/24/2019

</td>

<td style="text-align:left;">

Movimento 5 Stelle Party

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.garanteprivacy.it/web/guest/home/docweb/-/docweb-display/docweb/9101974>

</td>

<td style="text-align:left;">

Website affiliated with the Movimento 5 Stelle, an Italian political
party, had a data breach in 2017. Rousseau, the data processor running
these websites, had insufficient security measures in place. Garante,
the Italian Data Protection Authority, issued a request to update these
measures and the privacy information notice, for more transparency on
the processing of data. The information issue was completed on time.
However, Rousseau failed to adopt new security measures, and it was
fined by Garante.

</td>

</tr>

<tr>

<td style="text-align:right;">

43

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

9400

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

04/17/2019

</td>

<td style="text-align:left;">

Not disclosed

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-167-hatarozat.pdf>

</td>

<td style="text-align:left;">

The data controller had no legal basis to process data in conformity
with art 6.1.b, related to the claims.

</td>

</tr>

<tr>

<td style="text-align:right;">

44

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

510

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

04/08/2019

</td>

<td style="text-align:left;">

Medical service providers

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 9 (1) GDPR|Art. 9 (2) GDPR|Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element_view&aid=2192>

</td>

<td style="text-align:left;">

The medical centers unlawfully processed personal data of the subject
G.B. Software was used to generate the registration form for changing
the GP, and it went ahead to the Regional Health Insurance Fund. After
it arrived at another medical center, it was concluded that all parts
had taken part in the unlawful processing of data.

</td>

</tr>

<tr>

<td style="text-align:right;">

45

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

34375

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

04/05/2019

</td>

<td style="text-align:left;">

Political Party - Undisclosed

</td>

<td style="text-align:left;">

Art. 33 (1) GDPR|Art. 33 (5) GDPR|Art. 34 (1) GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.cms-lawnow.com/ealerts/2019/04/hungarian-data-authority-investigates-two-cases-of-privacy-breaches?cc_lang=en>

</td>

<td style="text-align:left;">

The political party did not notify the NAIH about a data breach.
Moreover, it didn’t document the data breach as per GDPR article 33.5.
Therefore, the political party received a fine of HUF 11.000.000
(equivalent to 34.375 EUR). The hacker behind the breach had used a
redirection attack on the official website of the political party and
disclosed information about more than 6.000 people.

</td>

</tr>

<tr>

<td style="text-align:right;">

46

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1900

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

04/04/2019

</td>

<td style="text-align:left;">

Not disclosed

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH_2019_133_hatarozat.pdf>

</td>

<td style="text-align:left;">

A data controller failed to allow the data subject access.

</td>

</tr>

<tr>

<td style="text-align:right;">

47

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/norway.svg>

</td>

<td style="text-align:left;">

Norway

</td>

<td style="text-align:right;">

170000

</td>

<td style="text-align:left;">

Norwegian Supervisory Authority (Datatilsynet)

</td>

<td style="text-align:left;">

03/29/2019

</td>

<td style="text-align:left;">

Bergen Municipality

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.datatilsynet.no/en/about-privacy/reports-on-specific-subjects/administrative-fine-of-170.000--imposed-on-bergen-municipality/>

</td>

<td style="text-align:left;">

The municipality had employed insufficient security measures in
protecting its computer systems. As a result, personal data related to
more than 35.000 individuals became publicly accessible. In the case of
a few schools, anyone could access information related to the staff, the
pupils, and the employees of the school. Moreover, the municipality has
received warnings about the weakness of its security measures before but
chose not to do anything.

</td>

</tr>

<tr>

<td style="text-align:right;">

48

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/republic-of-poland.svg>

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

219538

</td>

<td style="text-align:left;">

Polish National Personal Data Protection Office (UODO)

</td>

<td style="text-align:left;">

03/26/2019

</td>

<td style="text-align:left;">

Private company

</td>

<td style="text-align:left;">

Art. 14 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://uodo.gov.pl/en/553/1009>

</td>

<td style="text-align:left;">

The private company was fined for having breached the information
obligation in the case of personal data of several entrepreneurs. The
data was taken from public sources (Central Electronic Register and
Information on Economic Activity) and used for commercial purposes. In
accordance with Art. 14(1) – (3) of the GDPR, the company was obligated
to inform all the individuals concerned about the data processing.
However, the company informed only those individuals for whom it had
email addresses. For the rest, the high operational costs made them
ignore the information obligation.

</td>

</tr>

<tr>

<td style="text-align:right;">

49

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

5100

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

03/26/2019

</td>

<td style="text-align:left;">

A.P. EOOD

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element_view&aid=2191>

</td>

<td style="text-align:left;">

The personal data administrator unlawfully processed personal data of
subject D.D related to an Employment Contract, while the subject was
imprisoned.

</td>

</tr>

<tr>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/portugal.svg>

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Portuguese Data Protection Authority (CNPD)

</td>

<td style="text-align:left;">

03/25/2019

</td>

<td style="text-align:left;">

Not available

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.cnpd.pt/bin/decisoes/Delib/DEL_2019_222.pdf>

</td>

<td style="text-align:left;">

Not available

</td>

</tr>

<tr>

<td style="text-align:right;">

51

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

9704

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

03/21/2019

</td>

<td style="text-align:left;">

Not available

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 5 (1) e) GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34470>

</td>

<td style="text-align:left;">

The data processing had breached the storage limitation and data
minimization principles of the GDPR.

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

03/04/2019

</td>

<td style="text-align:left;">

Not disclosed

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 5 (1) c) GDPR|Art. 13 (3) GDPR|Art. 17 (1)
GDPR|Art. 6 (4) GDRP

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-2526-2-H-hatarozat.pdf>

</td>

<td style="text-align:left;">

The financial institution refused the data erasure request of a
customer, arguing that it was in the institution’s best interests to
retain the phone number, given that the customer had debts. However, the
NAIH argued that the creditor could communicate with the debtor by post,
and the phone number was unnecessary. The financial institution had
broken the data minimization and purpose limitation principles. A fine
was issued equal to 0.025% of the institution’s annual net revenue.

</td>

</tr>

<tr>

<td style="text-align:right;">

53

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Data Protection Authority of Berlin

</td>

<td style="text-align:left;">

03/01/2019

</td>

<td style="text-align:left;">

N26

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.handelsblatt.com/finanzen/banken-versicherungen/datenspeicherung-schwarze-listen-so-bekam-n26-aerger-mit-datenschuetzern/24204544.html?ticket=ST-74688114-wDx6jUqdgkknMT7CMoQe-ap5>

</td>

<td style="text-align:left;">

A bank had retained the personal data of former customers in order to
create a blacklist. Apparently, they wanted to prevent those customers
from opening up new accounts at their bank because they were suspected
of money laundering. While the bank wanted to hand-wave away this
unlawful act by appealing to the German Banking Act, the Berlin
Supervisory Authority found this to be illegal.

</td>

</tr>

<tr>

<td style="text-align:right;">

54

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

582

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

02/28/2019

</td>

<td style="text-align:left;">

Not available

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34466>

</td>

<td style="text-align:left;">

The data processing was conducted in a way that didn&\#8217;t ensure the
appropriate security of the data itself. Meaning that anyone could
access or alter it in an irreversible way (deletion, destruction).

</td>

</tr>

<tr>

<td style="text-align:right;">

55

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

02/28/2019

</td>

<td style="text-align:left;">

Kecskemét Mayor’s Office

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-596-hatarozat.pdf>

</td>

<td style="text-align:left;">

The fine was issued after the Mayor’s Office unlawfully disclosed
personal information related to a whistleblower. The individual
complained to the NAIH about his employer. Afterward, the company
requested information about the complaint, and the Mayor’s Office
“accidentally” released the name of the complainant. The individual
was fired as a result.

</td>

</tr>

<tr>

<td style="text-align:right;">

56

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

27100

</td>

<td style="text-align:left;">

Bulgarian Commission for Personal Data Protection (KZLD)

</td>

<td style="text-align:left;">

02/26/2019

</td>

<td style="text-align:left;">

Telecommunication service provider

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element_view&aid=2180>

</td>

<td style="text-align:left;">

The complainant was unlawfully and unknowingly been registered for the
prepaid services of a telecommunication service provider. The employees
had used personal data illegally and without express consent from the
subject. Moreover, the signature on the application was found to be
incongruent and dissimilar to the subject’s own signature. The identity
card number on the prepaid application was also fake.

</td>

</tr>

<tr>

<td style="text-align:right;">

57

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

776

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

02/26/2019

</td>

<td style="text-align:left;">

Not available

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34469>

</td>

<td style="text-align:left;">

Not available.

</td>

</tr>

<tr>

<td style="text-align:right;">

58

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgarian Commission for Personal Data Protection (KZLD)

</td>

<td style="text-align:left;">

02/22/2019

</td>

<td style="text-align:left;">

Unknown company

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element_view&aid=2177>

</td>

<td style="text-align:left;">

An employee requested access to his personal data from his employer. The
request was partially completed and delayed without justification.

</td>

</tr>

<tr>

<td style="text-align:right;">

59

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1560

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

02/20/2019

</td>

<td style="text-align:left;">

Unknown company

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-1841_hatarozat.pdf>

</td>

<td style="text-align:left;">

A data subject requested the erasure of the data processed by a debt
collector who requested further personal information to identify the
subject. After being provided with said information (place of birth,
mother’s maiden name, etc), the debt collector stated that he could not
comply with the request. The debt collector invoked the Accountancy Act
and other internal policies for why he was obliged to retain backup data
copies. The NAIH issued a fine because the data controller had not
informed the subject about these policies.

</td>

</tr>

<tr>

<td style="text-align:right;">

60

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/malta.svg>

</td>

<td style="text-align:left;">

Malta

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Data Protection Commissioner of Malta

</td>

<td style="text-align:left;">

02/18/2019

</td>

<td style="text-align:left;">

Lands Authority

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.gvzh.com.mt/malta-news/idpc-fines-lands-authority-data-breach/>

</td>

<td style="text-align:left;">

The Lands Authority had a data breach where 10 GB worth of personal data
was publicly accessible on the internet. The data contained sensitive
information about data subjects. The Data Protection Commissioner might
issue a fine of 25.000 Euros for each of the violations (data breaches).

</td>

</tr>

<tr>

<td style="text-align:right;">

61

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1560

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

02/08/2019

</td>

<td style="text-align:left;">

A bank

</td>

<td style="text-align:left;">

Art. 5 (1) d) GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019_363_hatarozat.pdf>

</td>

<td style="text-align:left;">

The bank erroneously sent the subject’s credit card data to another
person via SMS. Despite the data subject’s request to erase the data,
the bank continued to send SMS messages to that incorrect telephone
number. The bank was fined about 0.00016% of the annual net revenue.

</td>

</tr>

<tr>

<td style="text-align:right;">

62

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Data Protection Authority of Sachsen-Anhalt

</td>

<td style="text-align:left;">

02/05/2019

</td>

<td style="text-align:left;">

Private individual

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.mz-web.de/merseburg/hunderte-adressen-im-verteiler-merseburger-muss-fuer-wut-mails-ueber-2-000-euro-zahlen-32033308>

</td>

<td style="text-align:left;">

A private person sent several emails containing the email addresses of
several subjects, and each subject could see other recipients of that
email. In the person’s mailing list, more than 131 email addresses had
been found. He was accused of ten such offenses.

</td>

</tr>

<tr>

<td style="text-align:right;">

63

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/portugal.svg>

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Portuguese Data Protection Authority (CNPD)

</td>

<td style="text-align:left;">

02/05/2019

</td>

<td style="text-align:left;">

Not available

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cnpd.pt/bin/decisoes/Delib/DEL_2019_21.pdf>

</td>

<td style="text-align:left;">

Not available.

</td>

</tr>

<tr>

<td style="text-align:right;">

64

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1165

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

02/04/2019

</td>

<td style="text-align:left;">

Car renting company

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34465>

</td>

<td style="text-align:left;">

The company sold a card that was constantly tracked through GPS. The
owner found this out and reported it since the company had no
information related to this GPS tracking. The Czech Data Protection
Authority decreed that this was a violation of Art. 5 (1) of the GDPR,
and issued a fine.

</td>

</tr>

<tr>

<td style="text-align:right;">

65

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1165

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

02/04/2019

</td>

<td style="text-align:left;">

Credit brokerage

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34467>

</td>

<td style="text-align:left;">

The company did not process the data using the appropriate security
measures required to prevent unlawful alteration or destruction of the
data.

</td>

</tr>

<tr>

<td style="text-align:right;">

66

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/france.svg>

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

50000000

</td>

<td style="text-align:left;">

French Data Protection Authority (CNIL)

</td>

<td style="text-align:left;">

01/21/2019

</td>

<td style="text-align:left;">

Google Inc. 

</td>

<td style="text-align:left;">

Art. 13 GDPR|Art. 14 GDPR|Art. 6 GDPR|Art. 4 GDPR|Art. 5 GDPR

</td>

<td style="text-align:left;">

Several

</td>

<td style="text-align:left;">

<https://www.cnil.fr/en/cnils-restricted-committee-imposes-financial-penalty-50-million-euros-against-google-llc>

</td>

<td style="text-align:left;">

The French NGO “La Quadrature du Net” and the Austrian organization
“None Of Your Business” complained about the creation of a Google
account related to the configuration of the Android system in a mobile
phone. A fine of 50 million euros was issued because the following
principles were not observed: the principle of transparency (Art. 5
GDPR), the sufficiency of information (Art.13 / 14 GDPR), and the
presence of legal basis (Art. 6 GDPR).

</td>

</tr>

<tr>

<td style="text-align:right;">

67

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgarian Commission for Personal Data Protection (KZLD)

</td>

<td style="text-align:left;">

01/17/2019

</td>

<td style="text-align:left;">

Bank

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element&aid=1195>

</td>

<td style="text-align:left;">

The bank unlawfully came into possession of personal data related to a
student.

</td>

</tr>

<tr>

<td style="text-align:right;">

68

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

388

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

01/10/2019

</td>

<td style="text-align:left;">

Company

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34464>

</td>

<td style="text-align:left;">

The employer complained that the former company where he/she worked for
refused to delete personal information posted on Facebook. Even after
the termination of the employment contract, the company did not delete
the personal information of the former employee.

</td>

</tr>

<tr>

<td style="text-align:right;">

69

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

01/01/2019

</td>

<td style="text-align:left;">

Doctor

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://cyprus-mail.com/2019/10/11/doctor-fined-e14000-for-violating-patient-data-on-instagram/>

</td>

<td style="text-align:left;">

The data controller could not provide access to personal information to
a patient because the dossier could not be identified. The patient
complained to the Commissioner about this, and the hospital was fined
5.000 Euros.

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/austria.svg>

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

2200

</td>

<td style="text-align:left;">

Austrian Data Protection Authority (DSB)

</td>

<td style="text-align:left;">

12/20/2018

</td>

<td style="text-align:left;">

Private individual

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR|Art. 6 (1) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.ris.bka.gv.at/Dokumente/Dsk/DSBT_20181220_DSB_D550_037_0003_DSB_2018_00/DSBT_20181220_DSB_D550_037_0003_DSB_2018_00.pdf>

</td>

<td style="text-align:left;">

A person was fined for having unlawfully filmed public areas using a
private CCTV system. The system filmed parking lots, sidewalks, a garden
area of a nearby property, and it also filmed the neighbors going in and
out of their homes. The video surveillance was found to be unreasonable
given the initial purpose of the CCTV system itself. Because it filmed
private areas of life without the express consent of the people
involved, the subject was fined.

</td>

</tr>

<tr>

<td style="text-align:right;">

71

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

12/18/2018

</td>

<td style="text-align:left;">

Not disclosed

</td>

<td style="text-align:left;">

Art. 12 (4) GDPR|Art. 15 GDPR|Art. 18 (1) c) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2018-5559-H-hatarozat.pdf>

</td>

<td style="text-align:left;">

The data subject was not given access to CCTV recordings and was not
informed that he could complain to the supervisory authority about the
data controller’s refusal to retain the recordings.

</td>

</tr>

<tr>

<td style="text-align:right;">

72

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Data Protection Authority of Hamburg

</td>

<td style="text-align:left;">

12/17/2018

</td>

<td style="text-align:left;">

Kolibri Image Regina und Dirk Maass GbR

</td>

<td style="text-align:left;">

Art. 28 (3) GDPR

</td>

<td style="text-align:left;">

Failure to collect sufficient data processing consent

</td>

<td style="text-align:left;">

<https://www.heise.de/newsticker/meldung/DSGVO-5000-Euro-Bussgeld-fuer-fehlenden-Auftragsverarbeitungsvertrag-4282737.html>

</td>

<td style="text-align:left;">

This fine was apparently withdrawn. The case concerned the Kolibri Image
who lodged a complaint that a service provider did not want to sign a
processing agreement. Afterward, the Kolibri Image was fined because it
didn’t have any processing agreement with the service provider. However,
the company argued that the service provider was not a processor, and
therefore the fine was unreasonable and unwarranted.

</td>

</tr>

<tr>

<td style="text-align:right;">

73

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/austria.svg>

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

4800

</td>

<td style="text-align:left;">

Austrian Data Protection Authority (DSB)

</td>

<td style="text-align:left;">

12/12/2018

</td>

<td style="text-align:left;">

Bookmaker

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.dsb.gv.at/documents/22758/116802/Straferkenntnis+DSB-D550.038+0003-DSB+2018.pdf/fb0bb313-8651-44ac-a713-c286d83e3f19>

</td>

<td style="text-align:left;">

The betting place used a system of video surveillance illegally because
it filmed the public space (the sidewalk). Private individuals are not
allowed to do this on a large scale like in this case.

</td>

</tr>

<tr>

<td style="text-align:right;">

74

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

11/21/2018

</td>

<td style="text-align:left;">

Knuddels.de

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.baden-wuerttemberg.datenschutz.de/lfdi-baden-wuerttemberg-verhaengt-sein-erstes-bussgeld-in-deutschland-nach-der-ds-gvo/>

</td>

<td style="text-align:left;">

A hack revealed the personal data that included email addresses and
passwords of around 330,000 users.

</td>

</tr>

<tr>

<td style="text-align:right;">

75

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

388

</td>

<td style="text-align:left;">

Czech Data Protection Auhtority (UOOU)

</td>

<td style="text-align:left;">

10/25/2018

</td>

<td style="text-align:left;">

Not available

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34468>

</td>

<td style="text-align:left;">

Not available.

</td>

</tr>

<tr>

<td style="text-align:right;">

76

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/austria.svg>

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

300

</td>

<td style="text-align:left;">

Austrian Data Protection Authority (DSB)

</td>

<td style="text-align:left;">

09/27/2018

</td>

<td style="text-align:left;">

Private individual

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.ris.bka.gv.at/Dokumente/Dsk/DSBT_20180927_DSB_D550_084_0002_DSB_2018_00/DSBT_20180927_DSB_D550_084_0002_DSB_2018_00.pdf>

</td>

<td style="text-align:left;">

The car owner used the dash-cam unlawfully.

</td>

</tr>

<tr>

<td style="text-align:right;">

77

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/portugal.svg>

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

400000

</td>

<td style="text-align:left;">

Portuguese Data Protection Authority (CNPD)

</td>

<td style="text-align:left;">

07/17/2018

</td>

<td style="text-align:left;">

Hospital

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.cnpd.pt/index.asp>

</td>

<td style="text-align:left;">

The hospital was found to create fake doctor profiles for the personnel
to unlawfully access patient data. The management system found 985
registered doctors when the hospital only had 296 doctors.

</td>

</tr>

<tr>

<td style="text-align:right;">

78

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgarian Commission for Personal Data Protection (KZLD)

</td>

<td style="text-align:left;">

05/12/2018

</td>

<td style="text-align:left;">

Bank

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element_view&aid=2152>

</td>

<td style="text-align:left;">

The bank was fined 500 EUR for calling a client about the unresolved
bills of his neighbor. The client then invoked his right to be
forgotten, which the bank ignored at first. Another motion was started,
and the client complained to the KZLD. Apparently, the bank hadn’t
requested consent from the subject when processing his data.

</td>

</tr>

<tr>

<td style="text-align:right;">

79

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/austria.svg>

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

18000000

</td>

<td style="text-align:left;">

Austrian Data Protection Authority (DSB)

</td>

<td style="text-align:left;">

10/23/2019

</td>

<td style="text-align:left;">

Austrian Post

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://wien.orf.at/stories/3019396/>

</td>

<td style="text-align:left;">

The Austrian Post had sold detailed personal profiles of approximately
three million Austrians to various companies and political parties. The
profiles contained names, addresses, political predilections, and even
intimate details.

</td>

</tr>

<tr>

<td style="text-align:right;">

80

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

07/30/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.baden-wuerttemberg.datenschutz.de/wp-content/uploads/2019/07/PM-Datenschutzverletzungen-bereiten-zunehmend-Sorge-30.07.2019.pdf>

</td>

<td style="text-align:left;">

Two companies working in finances didn’t follow the procedure when
disposing of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

81

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

10/17/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.baden-wuerttemberg.datenschutz.de/wp-content/uploads/2019/07/PM-Datenschutzverletzungen-bereiten-zunehmend-Sorge-30.07.2019.pdf>

</td>

<td style="text-align:left;">

Because of insufficient data security mechanisms, a digital publication
accidentally disclosed personal health data related to several subjects.

</td>

</tr>

<tr>

<td style="text-align:right;">

82

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

14500000

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

10/30/2019

</td>

<td style="text-align:left;">

Deutsche Wohnen SE

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 25 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.lexology.com/library/detail.aspx?g=1e75e1a5-2bb6-409c-b1dd-239f51bdb2bd>

</td>

<td style="text-align:left;">

The company collected data from multiple tenants without providing the
option to remove that data once it was no longer required. This led to
the company retaining personal data of tenants for years (salary
statements, social security insurances, health insurances, tax
insurances, bank statements). The Berlin Data Commissioner issued a fine
of €14,500,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

83

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

36000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

10/25/2019

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00301-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Vodafone Espana called the complainant to offer its services but the
data subject refused. His personal data had been acquired by the company
through his daughter. Despite his refusal, Vodafone Espana provided the
services and demanded payment for them. Therefore, the company had
unlawfully processed the complainant’s personal data without express
consent.

</td>

</tr>

<tr>

<td style="text-align:right;">

84

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/republic-of-poland.svg>

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

220000

</td>

<td style="text-align:left;">

Personal Data Protection Office

</td>

<td style="text-align:left;">

03/29/2019

</td>

<td style="text-align:left;">

Bisnode

</td>

<td style="text-align:left;">

Art.14 GDPR

</td>

<td style="text-align:left;">

Non-compliance with the right of consent

</td>

<td style="text-align:left;">

<https://uodo.gov.pl/decyzje/ZSPR.421.3.2018>

</td>

<td style="text-align:left;">

The Company failed to observe Art.14 of the GDPR, which states that the
data controller must inform the data subject of the processing of
personal data. The DPA has stated that Bisnode has three months to
notify a total of 6 million people of this.

</td>

</tr>

<tr>

<td style="text-align:right;">

85

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/united-kingdom.svg>

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

100000

</td>

<td style="text-align:left;">

Information Commissioner

</td>

<td style="text-align:left;">

06/24/2019

</td>

<td style="text-align:left;">

EE

</td>

<td style="text-align:left;">

Art.14 GDPR

</td>

<td style="text-align:left;">

Non-compliance with the right of consent

</td>

<td style="text-align:left;">

<https://ico.org.uk/action-weve-taken/enforcement/ee-limited/>

</td>

<td style="text-align:left;">

The Company sent marketing messages to over 2.5 million customers
without their consent. The marketing message encouraged data subjects
the &\#8220;My EE&\#8221; app to manage their accounts. Furthermore, the
Company sent another batch of marketing messages to other customers
afterward.

</td>

</tr>

<tr>

<td style="text-align:right;">

86

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/united-kingdom.svg>

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Information Commissioner

</td>

<td style="text-align:left;">

07/10/2019

</td>

<td style="text-align:left;">

Driver and Vehicle Licensing Agency (DVLA)

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Non-compliance (Data Breach)

</td>

<td style="text-align:left;">

<https://www.autoexpress.co.uk/car-news/consumer-news/91275/dvla-sale-of-driver-details-to-private-parking-firms-looked-at-by>

</td>

<td style="text-align:left;">

The Company shared personal driver details with other third-parties,
including parking firms.

</td>

</tr>

<tr>

<td style="text-align:right;">

87

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/united-kingdom.svg>

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Information Commissioner

</td>

<td style="text-align:left;">

07/16/2019

</td>

<td style="text-align:left;">

Life at Parliament View

</td>

<td style="text-align:left;">

Data Protection Act 2018

</td>

<td style="text-align:left;">

Non-compliance (Data Breach)

</td>

<td style="text-align:left;">

<https://ico.org.uk/about-the-ico/news-and-events/news-and-blogs/2019/07/estate-agency-fined-80-000-for-failing-to-keep-tenants-data-safe/>

</td>

<td style="text-align:left;">

The Company experienced the data breach when it transferred the personal
data of 18,610 customers to a partner organization. In doing so, the
company allowed anyone to access the personal data because the
“Anonymous Authentication” function was switched on. The data breach
was active for two years.

</td>

</tr>

<tr>

<td style="text-align:right;">

88

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/united-kingdom.svg>

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

140000

</td>

<td style="text-align:left;">

Information Commissioner

</td>

<td style="text-align:left;">

02/01/2019

</td>

<td style="text-align:left;">

Leave.EU & GoSkippy

</td>

<td style="text-align:left;">

Art.14 of the GDPR

</td>

<td style="text-align:left;">

Non-compliance with the right of consent

</td>

<td style="text-align:left;">

<https://www.bbc.com/news/uk-politics-46109883>

</td>

<td style="text-align:left;">

Leave.EU subscriber emails contained marketing ads related to the
GoSkippy services of the Eldon Insurance firm. The data subjects did not
give their consent to this, hence the fine issued by the ICO.

</td>

</tr>

<tr>

<td style="text-align:right;">

89

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Data Protection Authority

</td>

<td style="text-align:left;">

01/23/2019

</td>

<td style="text-align:left;">

Small shipping company

</td>

<td style="text-align:left;">

Art. 28 of the GDPR

</td>

<td style="text-align:left;">

<https://dataprivacy.foxrothschild.com/2019/01/articles/european-union/hessian-dpa-fines-shipping-company-for-missing-data-processing-agreement/>

</td>

<td style="text-align:left;">

The data controller company lacked a data processing agreement with the
Spanish service provider.

</td>

<td style="text-align:left;">

The data controller company lacked a data processing agreement with the
Spanish service provider.

</td>

</tr>

<tr>

<td style="text-align:right;">

90

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 5 (1) d) GDPR

</td>

<td style="text-align:left;">

Failure to comply with processing principles

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00331-2018_ORI.pdf>

</td>

<td style="text-align:left;">

Vodafone mistakenly charged a customer whose information it disclosed to
BADEXCUG, a solvency registry. SETSTI, the Spanish telecommunications
and information agency demanded that Vodafone reimburse the client. The
AEPD decided that Vodafone had acted erroneously and that it had
breached the principle of accuracy.

</td>

</tr>

<tr>

<td style="text-align:right;">

91

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Gestion De Cobros Yo Cobro SL

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00121-2019_ORI.pdf>

</td>

<td style="text-align:left;">

An online credit agency transferred an undue credit claim to a debt
collecting agency, providing the agency with the subject’s email
address. However, the debt collecting agency sent emails to the company
where the subject worked. This institutional email was accessible by all
employees of that company. The online credit agency had not provided
these emails.

</td>

</tr>

<tr>

<td style="text-align:right;">

92

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

27000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 5 (1) d) GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00411-2018_ORI.pdf>

</td>

<td style="text-align:left;">

The data subject had demanded that his data be deleted from the Vodafone
records in 2015, which the company agreed to and confirmed. However, he
received more than 200 SMS messages in 2018, which Vodafone admitted it
was a technical error on their part. They had performed tests, and the
data subject’s phone number mistakenly appeared in various customer
files. The fine was set at 27.000 Euros since Vodafone admitted to its
mistake.

</td>

</tr>

<tr>

<td style="text-align:right;">

93

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

ENDESA

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00074-2019_ORI.pdf>

</td>

<td style="text-align:left;">

ENDESA erroneously charged the claimant’s bank account, as the
beneficiary of the energy supply company’s services was a third party.
Upon request of the claimant that her data be deleted, ENDESA handled
the data improperly and mistakenly sent it to the third party.
Therefore, the AEPD considered that ENDESA had breached the
confidentiality principle. It’s worth noting that the third party had
been given a 2-year restraining order regarding the data subject.

</td>

</tr>

<tr>

<td style="text-align:right;">

94

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

12000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Restaurant

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://theword.iuslaboris.com/hrlaw/insights/spain-video-surveillance-and-data-protection-in-the-workplace>

</td>

<td style="text-align:left;">

The restaurant wanted to sanction an employee using images taken by
another employee in the restaurant, to be used as evidence.

</td>

</tr>

<tr>

<td style="text-align:right;">

95

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/slovakia.svg>

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovak Data Protection Office

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.dataprotection.gov.sk/uoou/sites/default/files/sprava_o_stave_ochrany_osobnych_udajov_za_obdobie_25.maj_2018_az_24_maj_2019.pdf>

</td>

<td style="text-align:left;">

The data controller did not comply with the data subject’s request to
access personal data related to audio recordings.

</td>

</tr>

<tr>

<td style="text-align:right;">

96

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/slovakia.svg>

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovak Data Protection Office

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.gov.sk/uoou/sites/default/files/sprava_o_stave_ochrany_osobnych_udajov_za_obdobie_25.maj_2018_az_24_maj_2019.pdf>

</td>

<td style="text-align:left;">

Personal data in the form of documents were thrown to the garbage dump,
which is an improper method of disposing of such documents.

</td>

</tr>

<tr>

<td style="text-align:right;">

97

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/slovakia.svg>

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovak Data Protection Office

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Unknwon

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.gov.sk/uoou/sites/default/files/sprava_o_stave_ochrany_osobnych_udajov_za_obdobie_25.maj_2018_az_24_maj_2019.pdf>

</td>

<td style="text-align:left;">

Improper information security measures in place.

</td>

</tr>

<tr>

<td style="text-align:right;">

98

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/slovakia.svg>

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovak Data Protection Office

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 (1) a) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dataprotection.gov.sk/uoou/sites/default/files/sprava_o_stave_ochrany_osobnych_udajov_za_obdobie_25.maj_2018_az_24_maj_2019.pdf>

</td>

<td style="text-align:left;">

The Data Protection Authority found that the City had published personal
data on the official city website in violation of the law, and without
asking for the consent of the data subjects. The City had claimed that
it was doing this attempting to fulfill the Freedom of Information Act’s
premises.

</td>

</tr>

<tr>

<td style="text-align:right;">

99

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/slovakia.svg>

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovak Data Protection Office

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

<https://www.etrend.sk/ekonomika/gdpr-zacina-hryzt-telekomunikacny-operator-dostal-pokutu-40-tisic-eur.html>

</td>

<td style="text-align:left;">

The data controller didn’t properly impose the necessary security
measures needed to protect personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

100

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Data Protection Authority of Hamburg

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.pingdigital.de/blog/2019/03/29/implodierende-aufsichtsbehoerden/1626>

</td>

<td style="text-align:left;">

Not available.

</td>

</tr>

<tr>

<td style="text-align:right;">

101

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Data Protection Authority of Saarland

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://indd.adobe.com/view/d639298c-3165-4e30-85d8-0730de2a3598>

</td>

<td style="text-align:left;">

Illegal disclosure of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

102

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Data Protection Authority of Hamburg

</td>

<td style="text-align:left;">

12/01/2018

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 83 (4) a) GDPR|Art. 33 (1) GDPR|Art. 34 (1) GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://datenschutz-hamburg.de/assets/pdf/27._Taetigkeitsbericht_Datenschutz_2018_HmbBfDI.pdf>

</td>

<td style="text-align:left;">

A data breach was not notified in time and the affected subjects were
not made aware.

</td>

</tr>

<tr>

<td style="text-align:right;">

103

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

07/30/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.baden-wuerttemberg.datenschutz.de/wp-content/uploads/2019/07/PM-Datenschutzverletzungen-bereiten-zunehmend-Sorge-30.07.2019.pdf>

</td>

<td style="text-align:left;">

In a digital publication, health data was accidentally published due to
inadequate internal control mechanisms.Due to inadequate internal
control mechanisms, health data was made public by a digital
publication.

</td>

</tr>

<tr>

<td style="text-align:right;">

104

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Data Protection Authority of Berlin

</td>

<td style="text-align:left;">

10/01/2019

</td>

<td style="text-align:left;">

Deutsche Wohnen SE

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 25 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.datenschutz-berlin.de/fileadmin/user_upload/pdf/pressemitteilungen/2019/20191105-PM-Bussgeld_DW.pdf>

</td>

<td style="text-align:left;">

Further fines of between €6,000 and €17,000 were issues to the company
due to the faulty storage of personal data. See the separate entry about
Deutsche Wohnen SE.

</td>

</tr>

<tr>

<td style="text-align:right;">

105

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/austria.svg>

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

1800

</td>

<td style="text-align:left;">

Austrian Data Protection Authority (DSB)

</td>

<td style="text-align:left;">

11/01/2018

</td>

<td style="text-align:left;">

Restaurant

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

<https://apps.derstandard.de/privacywall/story/2000092017999/erst-vier-strafen-wegen-dsgvo-seit-mai>

</td>

<td style="text-align:left;">

CCTV footage was not properly used.

</td>

</tr>

<tr>

<td style="text-align:right;">

106

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

03/01/2019

</td>

<td style="text-align:left;">

State Hospital

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.agplaw.com/cyprus-gdpr-commissioner-fines-newspaper-and-hospital/>

</td>

<td style="text-align:left;">

The data controller could not provide access to personal information to
a patient because the dossier could not be identified. The patient
complained to the Commissioner about this, and the hospital was fined
5.000 Euros.

</td>

</tr>

<tr>

<td style="text-align:right;">

107

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/denmark.svg>

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

160000

</td>

<td style="text-align:left;">

Danish Data Protection Authority (Datatilsynet)

</td>

<td style="text-align:left;">

03/01/2019

</td>

<td style="text-align:left;">

Taxa 4x35

</td>

<td style="text-align:left;">

Art. 5(1) e) GDPR

</td>

<td style="text-align:left;">

Failure to comply with processing principles

</td>

<td style="text-align:left;">

<https://www.computerworld.dk/art/246918/datatilsynet-melder-foerste-danske-selskab-til-politiet-for-brud-paa-gdpr-kraever-boede-paa-1-2-millioner-kroner>

</td>

<td style="text-align:left;">

The taxi company was discovered having over 9 million person records
that it stored unlawfully. Because the company hadn’t deleted this
personal data, the Danish Data Protection Authority issued a fine.

</td>

</tr>

<tr>

<td style="text-align:right;">

108

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/netherlands.svg>

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

900000

</td>

<td style="text-align:left;">

Dutch Supervisory Authority for Data Protection (AP)

</td>

<td style="text-align:left;">

10/31/2019

</td>

<td style="text-align:left;">

UWV - Insurance provider

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://autoriteitpersoonsgegevens.nl/nl/nieuws/ap-dwingt-uwv-met-sanctie-gegevens-beter-te-beveiligen>

</td>

<td style="text-align:left;">

The Dutch employee insurance service provider &\#8211;
&\#8220;Uitvoeringsinstituut Werknemersverzekeringen &\#8211; UWV did
not use multi-factor authentication for accessing the employer web
portal. Health and safety services, as well as employers, were able to
view and collect data from employees, data to which normally they should
not have had access to.

</td>

</tr>

<tr>

<td style="text-align:right;">

109

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/republic-of-poland.svg>

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

47000

</td>

<td style="text-align:left;">

Polish National Personal Data Protection Office (UODO)

</td>

<td style="text-align:left;">

10/16/2019

</td>

<td style="text-align:left;">

ClickQuickNow

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Failure to comply with processing principles

</td>

<td style="text-align:left;">

<https://edpb.europa.eu/news/national-news/2019/polish-dpa-withdrawal-consent-shall-not-be-impeded_en>

</td>

<td style="text-align:left;">

The Company did not have the appropriate organizational measures in
place that would allow data subjects to withdraw their consent to the
processing of personal data. Moreover, the data subjects also couldn’t
easily request the deletion of their personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

110

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

12000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/21/2019

</td>

<td style="text-align:left;">

Madrileña Red de Gas

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00188-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The gas company did not have the necessary technical measures in place
to verify the identity of the subjects&\#8217; data. It was alleged by a
third party that the company emailed their information to a third party
in regards to a request.

</td>

</tr>

<tr>

<td style="text-align:right;">

111

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

10/31/2019

</td>

<td style="text-align:left;">

Jocker Premium Invex

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00291-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Postal advertisements and commercial offers were sent by Jocker Premium
Invex to a registrant to a local census, even though the registrant did
not consent to receive such advertisements and offers.

</td>

</tr>

<tr>

<td style="text-align:right;">

112

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/06/2019

</td>

<td style="text-align:left;">

Cerrajero Online

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00266-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The company collected personal data without accurate information
regarding the collection of this data.

</td>

</tr>

<tr>

<td style="text-align:right;">

113

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

900

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/07/2019

</td>

<td style="text-align:left;">

TODOTECNICOS24H S.L.

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00268-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The company TODOTECNICOS24H collected personal data without accurate
information regarding the collection of this data.

</td>

</tr>

<tr>

<td style="text-align:right;">

114

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/13/2019

</td>

<td style="text-align:left;">

General Confederation of Labour

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00174-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The General Confederation of Labour emailed personal data of a
complainant with the aim of organizing a meeting. This included the
name, home address, relationship status, pregnancy status and the date
of an ongoing harassment case. The email was sent to around 400 members
of the organization with the affected individual&\#8217;s consent.

</td>

</tr>

<tr>

<td style="text-align:right;">

115

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/14/2019

</td>

<td style="text-align:left;">

Telefónica SA

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00251-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A person was charged by the phone operator Telefónica for a telephone
service that they never requested and owned. This happened because the
bank account of the affected person was linked to the Telefónica profile
of another person and as such the fees for the service were deduced from
the affected person&\#8217;s account. The AEDP ruled that this was
against the principles described by article 5 of GDPR.

</td>

</tr>

<tr>

<td style="text-align:right;">

116

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/19/2019

</td>

<td style="text-align:left;">

Corporacion RTVE

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00305-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Corporacion de Radio y Television Espanola lost 6 USB sticks with
unencrypted personal information and data.

</td>

</tr>

<tr>

<td style="text-align:right;">

117

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/21/2019

</td>

<td style="text-align:left;">

Viaqua Xestión SA

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00233-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A third party had access to and modified the personal data of a customer
that was included in a contract. The third party had no legal basis to
access the data.

</td>

</tr>

<tr>

<td style="text-align:right;">

118

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

11/22/2019

</td>

<td style="text-align:left;">

BNP Paribas SA

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 17 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=Amenda_pentru_incalcarea_RGPD&lang=ro>

</td>

<td style="text-align:left;">

BNP Paribas Personal Finance was requested to erase personal data of a
client and it did not do so during the timeframe required by GDPR
legislation.

</td>

</tr>

<tr>

<td style="text-align:right;">

119

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

11000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

11/25/2019

</td>

<td style="text-align:left;">

Fan Courier Express SRL

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=O_noua_amenda_in_baza_RGPD&lang=ro>

</td>

<td style="text-align:left;">

Fan Courier Express SRL, which is a national courier service, was given
an €11,000 fine because it failed to take appropriate technical and
organizational measures to prevent the loss of personal data (name, bank
card number, CVV code, cardholder&\#8217;s address, personal
identification number, serial and identity card number, bank account
number, authorized credit limit) of over 1100 private individuals.

</td>

</tr>

<tr>

<td style="text-align:right;">

120

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/france.svg>

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

500000

</td>

<td style="text-align:left;">

French Data Protection Authority (CNIL)

</td>

<td style="text-align:left;">

11/21/2019

</td>

<td style="text-align:left;">

Futura Internationale

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 13 GDPR|Art. 14 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.legifrance.gouv.fr/affichCnil.do?oldAction=rechExpCnil&id=CNILTEXT000039419459&fastReqId=461698027&fastPos=1>

</td>

<td style="text-align:left;">

Futura Internationale was fined because after several individuals have
complained that they were cold-called by the company even after they
have expressly requested not to be called again. The reason why the fine
was so high relative to similar cases and fines was that the CNIL
determined that the company had received a large number of letters
requesting to be taken off from the call lists but decided to ignore
them. More so, Futura Internationale was found to store excessive
information about customers and their health data. The company did also
not inform their customers about the processing of their personal data
and that all telephone conversations were recorded.

</td>

</tr>

<tr>

<td style="text-align:right;">

121

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/latvia.svg>

</td>

<td style="text-align:left;">

Latvia

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Data State Inspectorate (DSI)

</td>

<td style="text-align:left;">

11/01/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://twitter.com/900sekundes/status/1199208013959172096>

</td>

<td style="text-align:left;">

No concrete details have been released at this point other than a fine
of €150,000 was imposed in November 2019. We will update this card once
further information emerges.

</td>

</tr>

<tr>

<td style="text-align:right;">

122

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

03/01/2019

</td>

<td style="text-align:left;">

Newspaper

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.agplaw.com/cyprus-gdpr-commissioner-fines-newspaper-and-hospital/>

</td>

<td style="text-align:left;">

A newspaper was fined €10,000 after it had published both in electronic
and physical form the names and pictures of three police investigators.
The Cypriot Data Protection Commissioner considered that it would have
been enough to publish only the initials of the police officers or
photographs from which it would not have been possible to identify the
three officials, such as using blurred faces.

</td>

</tr>

<tr>

<td style="text-align:right;">

123

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

3140

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

UniCredit Bank

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.uoou.cz/kontrola-zpracovani-osobnich-udaju-bankou-unicredit-bank-czech-republic-and-slovakia-a-s/ds-5705/archiv=0&p1=5653>

</td>

<td style="text-align:left;">

UniCredit Bank opened a bank account for a person who has not requested
any account to be opened. The bank allegedly had his personal data at
their disposal because the affected person was responsible for closing a
bank account operated by his employer. The bank was requested to prove
that it had consent from the data subject to process his personal data
but was unable to provide this proof.

</td>

</tr>

<tr>

<td style="text-align:right;">

124

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

588

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Alza.cz a.s.

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 7 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.uoou.cz/kontrola-zpracovani-osobnich-udaju-po-odvolani-souhlasu-spolecnost-alza-cz-a-s/ds-5717/archiv=0&p1=5653>

</td>

<td style="text-align:left;">

The company acquired a photocopy of a person&\#8217;s ID card with the
person&\#8217;s consent but continued to use and process the personal
data even after the affected person had withdrawn their consent.

</td>

</tr>

<tr>

<td style="text-align:right;">

125

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

980

</td>

<td style="text-align:left;">

Czech Data Protection Authority (UOOU)

</td>

<td style="text-align:left;">

01/01/1970

</td>

<td style="text-align:left;">

Individual entrepreneur

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.uoou.cz/kontrola-zabezpeceni-osobnich-udaju-pri-provozovani-online-hry-fyzicka-osoba-podnikajici/ds-5723/archiv=0&p1=5653>

</td>

<td style="text-align:left;">

An online game operator was exposed to a DDoS attack that led to the
malfunctioning of the game serves. The attackers blackmailed the
operator into paying money for the attacks to stop. As part of the
&\#8220;deal&\#8221;, the attackers offered the operator to create and
implement a better firewall protection system that would prevent any
future attacks from other parties. The operator agreed to this
&\#8220;deal&\#8221;. The game operator then implemented the new code
which indeed proved to be better than the old one used but &\#8211;
let&\#8217;s be honest, unsurprisingly &\#8211; also included a backdoor
that allowed the attacker to steal all the data that was on the server
which included player details and personal information. The attacker
uploaded this information on their website after that.

</td>

</tr>

<tr>

<td style="text-align:right;">

126

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/portugal.svg>

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Portuguese Data Protection Authority (CNPD)

</td>

<td style="text-align:left;">

03/19/2019

</td>

<td style="text-align:left;">

Not known

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.cnpd.pt/bin/decisoes/Delib/DEL_2019_207.pdf>

</td>

<td style="text-align:left;">

No further information is available.

</td>

</tr>

<tr>

<td style="text-align:right;">

127

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/19/2019

</td>

<td style="text-align:left;">

Xfera Moviles S.A.

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00237-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A private individual received an SMS from Xfera Móviles which was
actually addressed to a different person and which included personal
details of that third party person. The information included personal
details as well as login details to the Xfera Móviles website for the
third party person.

</td>

</tr>

<tr>

<td style="text-align:right;">

128

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/slovakia.svg>

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Slovak Data Protection Office

</td>

<td style="text-align:left;">

09/27/2019

</td>

<td style="text-align:left;">

Slovak Telekom

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.etrend.sk/ekonomika/gdpr-zacina-hryzt-telekomunikacny-operator-dostal-pokutu-40-tisic-eur.html>

</td>

<td style="text-align:left;">

The data controller did not take the necessary technical measures to
prevent a data breach. No further details have been disclosed.

</td>

</tr>

<tr>

<td style="text-align:right;">

129

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/slovakia.svg>

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Slovak Data Protection Office

</td>

<td style="text-align:left;">

11/13/2019

</td>

<td style="text-align:left;">

Social Insurance Agency

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.etrend.sk/ekonomika/socialna-poistovna-porusila-gdpr-pokutu-50-tisic-eur-nechce-zaplatit.html>

</td>

<td style="text-align:left;">

Applications that were received from Slovak citizens requesting social
benefits were sent to foreign authorities by post. These were lost,
which resulted in all the personal details of the affected people to
become public, including their physical addresses.

</td>

</tr>

<tr>

<td style="text-align:right;">

130

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

11/29/2019

</td>

<td style="text-align:left;">

SC CNTAR TAROM SA

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Sanctiune_CN_TAROM&lang=ro>

</td>

<td style="text-align:left;">

A fine of €20,000 was issued to the Romanian national airline Tarom
because it failed to implement the necessary technical measures to
ensure the security of personal information. As a consequence of these
inadequate measures, a Tarom employee was able to access the flight
booking application without authorization and see the personal data of
22 passengers, after which the employee took a photo of the list and
made it public online.

</td>

</tr>

<tr>

<td style="text-align:right;">

131

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

11/29/2019

</td>

<td style="text-align:left;">

ING Bank N.V. Amsterdam

</td>

<td style="text-align:left;">

Art. 25 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Amenda_ING_RGPD&lang=ro>

</td>

<td style="text-align:left;">

The Romanian branch of ING Bank N.V. Amsterdam was fined with €80,000
due to not respecting data protection principles (privacy by design și
privacy by default) by not implementing adequate technical measures to
ensure the protection of personal data. As a consequence of this, a
total of 225,525 had their transactions doubled on debit card payments
during the period of 8-10 October 2018.This is one of the bigger fines
in Romania, but it&\#8217;s interesting to note that for similar
offenses in other countries fines of over several millions of Euros are
usually awarded. This denotes again the fact that different countries
have different approaches to GDPR enforcement.

</td>

</tr>

<tr>

<td style="text-align:right;">

132

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

11/29/2019

</td>

<td style="text-align:left;">

Royal President S.R.L.

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 15 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=alta_sanctiune_Royal_President&lang=ro>

</td>

<td style="text-align:left;">

The pension Royal President near Bucharest was fined €2,500 after it
refused to process a request for the exercise of the right of access.
The Romanian Data Processing Authority also determined that
customers&\#8217; personal data was not processed in accordance with
GDPR principles.

</td>

</tr>

<tr>

<td style="text-align:right;">

133

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/belgium.svg>

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Belgian Data Protection Authority (APD)

</td>

<td style="text-align:left;">

11/28/2019

</td>

<td style="text-align:left;">

City councilor

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.autoriteprotectiondonnees.be/news/la-chambre-contentieuse-sanctionne-deux-candidats-aux-elections-communales-de-2018>

</td>

<td style="text-align:left;">

Two Belgian politicians, a city councilor and a mayor have been fined
€5,000 each for sending out campaign emails to recipients who have not
consented to receive such emails.

</td>

</tr>

<tr>

<td style="text-align:right;">

134

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/belgium.svg>

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Belgian Data Protection Authority (APD)

</td>

<td style="text-align:left;">

11/28/2019

</td>

<td style="text-align:left;">

Mayor

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.autoriteprotectiondonnees.be/news/la-chambre-contentieuse-sanctionne-deux-candidats-aux-elections-communales-de-2018>

</td>

<td style="text-align:left;">

Two Belgian politicians, a city councilor and a mayor have been fined
€5,000 each for sending out campaign emails to recipients who have not
consented to receive such emails.

</td>

</tr>

<tr>

<td style="text-align:right;">

135

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

12/02/2019

</td>

<td style="text-align:left;">

Ikea Ibérica

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00127-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Ikea Ibérica was found to have installed cookies on a customer&\#8217;s
device without asking for permission.

</td>

</tr>

<tr>

<td style="text-align:right;">

136

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

105000

</td>

<td style="text-align:left;">

Data Protection Authority of Rheinland-Pfalz

</td>

<td style="text-align:left;">

12/03/2019

</td>

<td style="text-align:left;">

Rheinland-Pfalz Hospital

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.datenschutz.rlp.de/de/aktuelles/detail/news/detail/News/geldbusse-gegen-krankenhaus-aufgrund-von-datenschutz-defiziten-beim-patientenmanagement/>

</td>

<td style="text-align:left;">

The Data Protection Authority of Rheinland-Pfalz issued a fine of
€105,000 after a hospital after a mixup of patients. As a consequence
of this, wrong invoices were issues to the patients that released
sensitive personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

137

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

The Federal Commissioner for Data Protection and Freedom of Information
(BfDI)

</td>

<td style="text-align:left;">

12/09/2019

</td>

<td style="text-align:left;">

Rapidata GmbH

</td>

<td style="text-align:left;">

Art. 37 GDPR

</td>

<td style="text-align:left;">

No data protection officer appointed

</td>

<td style="text-align:left;">

<https://www.bfdi.bund.de/DE/Infothek/Pressemitteilungen/2019/30_BfDIverh%C3%A4ngtGeldbu%C3%9Fe1u1.html>

</td>

<td style="text-align:left;">

The Federal Commissioner for Data Protection and Freedom of Information
(BfDI) ha repeatedly requested the company to appoint a data protection
officer in accordance with Article 37 GDPR but even so, Rapidata GmbH
refused to do so. The company was fined with €10,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

138

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

9550000

</td>

<td style="text-align:left;">

The Federal Commissioner for Data Protection and Freedom of Information
(BfDI)

</td>

<td style="text-align:left;">

12/09/2019

</td>

<td style="text-align:left;">

1&1 Telecom GmbH

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.bfdi.bund.de/DE/Infothek/Pressemitteilungen/2019/30_BfDIverh%C3%A4ngtGeldbu%C3%9Fe1u1.html>

</td>

<td style="text-align:left;">

The telecom company 1&\#038;1 Telecom GmbH was fined with €9,550,000
after it came to light that sensitive customer information could be
obtained by phone by anyone by just telling a client&\#8217;s name and
date of birth. This could have permitted anyone to obtain the personal
information of any customer in case they knew their name and date of
birth. The BfDI considered that the company failed to implement the
necessary technical measures to ensure the protection of personal data.
The BfDI further revealed that the fine was intended to be much larger
but was eventually decreased due to the cooperation of the company
during the investigation.

</td>

</tr>

<tr>

<td style="text-align:right;">

139

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

28160

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

07/24/2019

</td>

<td style="text-align:left;">

Debt collection agency

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element_view&aid=2226>

</td>

<td style="text-align:left;">

A private individual complained to the Data Protection Commission of
Bulgaria (KZLD) that a debt collection agency has information about her
accounts and status of those accounts with the purpose of collecting tax
owned by the complainant. The KZLD concluded that the agency had no
legal basis to obtain and process the data.

</td>

</tr>

<tr>

<td style="text-align:right;">

140

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

15100

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

10/01/2019

</td>

<td style="text-align:left;">

Town of Kerepes

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-2076-hatarozat.pdf>

</td>

<td style="text-align:left;">

The Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH) concluded that the local authority of the city of
Kerepes didn&\#8217;t follow GDPR regulations when it installed a
security camera. The Authority explained that the processing of the data
was not in accordance with provisions of the GDPR.

</td>

</tr>

<tr>

<td style="text-align:right;">

141

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/28/2019

</td>

<td style="text-align:left;">

Curenergía Comercializador de último recurso

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00140-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A private individual complained that the company had used their personal
data that included their first and last name, address and VAT number in
order to open an electricity supply contract. The individual was a
former customer of the company, and as such the company was not allowed
anymore to reuse the former customer&\#8217;s data without their
permission.

</td>

</tr>

<tr>

<td style="text-align:right;">

142

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

21000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

10/19/2019

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00087-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Vodafone had processed personal data of the claimant (bank details,
name, surname and national identification number) years after the
contractual relationsid had ended. The fine of EUR 35.000 was reduced to
EUR 21.000.Vodafone processed the personal details of a former client,
details that included first name, last name and national ID number,
several years after their contractual relationship had ended. The
initial fine was set at €35,000 but it was reduced to €21,000 due to
cooperation on behalf of Vodafone Espana.

</td>

</tr>

<tr>

<td style="text-align:right;">

143

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

36000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

04/01/2019

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00092-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The company had sent a number of emails to a significant number of
recipients without using the BCC feature that would have hid the email
addresses of all the recipients from each other. The original fine was
set at €60,000 but reduced to €36,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

144

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

06/24/2019

</td>

<td style="text-align:left;">

Vodafone ONO

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00212-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A technical error allowed customers to view the personal data of other
customers on the company&\#8217;s website&\#8217;s customer area. The
original fine of €60,000 was reduced to €48,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

145

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

06/24/2019

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00205-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The personal data of a customer was disclosed to a different customer
through SMS. The original fine of €50,000 was reduced to €20,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

146

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

03/22/2019

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00064-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The claimant was charged for a Netflix subscription that they had not
requested. The claimant proved that the Netflix subscription was created
by a different person who obtained the personal details of the claimant
from Vodafone. The AEPD argued that the claimant did not consent to be
charged the Netflix subscription, and as such had fined Vodafone with
€40,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

147

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

3600

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

04/15/2019

</td>

<td style="text-align:left;">

AMADOR RECREATIVOS, S.L

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00135-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The games lounge was fined because it improperly used and processed
surveillance footage data.

</td>

</tr>

<tr>

<td style="text-align:right;">

148

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

05/08/2019

</td>

<td style="text-align:left;">

Private individual

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00050-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Video surveillance was used to monitor employees.

</td>

</tr>

<tr>

<td style="text-align:right;">

149

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

04/08/2019

</td>

<td style="text-align:left;">

Private individual

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00150-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Video surveillance was used to monitor employees.

</td>

</tr>

<tr>

<td style="text-align:right;">

150

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

05/06/2019

</td>

<td style="text-align:left;">

Telefónica SA

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00173-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A customer complained that their bank account was charged for two
invoices for the services the customer has purchased but on the
invoices, the personal details of a third party person were displayed.
Initially, the fine was determined to be €60,000 but was reduced to
€48,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

151

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/belgium.svg>

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Belgian Data Protection Authority (APD)

</td>

<td style="text-align:left;">

12/16/2019

</td>

<td style="text-align:left;">

Nursing Care Organization

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 15 GDPR|Art. 17 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.autoriteprotectiondonnees.be/sites/privacycommission/files/documents/DEQF_13-2019_FR_ANO.pdf>

</td>

<td style="text-align:left;">

A nursing care organization failed to act on a request by a data subject
to receive access to their data with the scope of erasing it.

</td>

</tr>

<tr>

<td style="text-align:right;">

152

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

12/11/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-51-hatarozat.pdf>

</td>

<td style="text-align:left;">

A company did not delete a former employee&\#8217;s private emails and
as such, it was determined that it processed private data without a
legal basis. The company also was found to exceed data retention
requirements. As per Hungarian laws, the name of the fined company was
not disclosed by the national data protection authority.

</td>

</tr>

<tr>

<td style="text-align:right;">

153

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

90

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

11/18/2019

</td>

<td style="text-align:left;">

Hospital

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-5112-hatarozat.pdf>

</td>

<td style="text-align:left;">

The hospital unlawfully charged a copying fee from the patient and
violated their right to access data.

</td>

</tr>

<tr>

<td style="text-align:right;">

154

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/hungary.svg>

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

7400

</td>

<td style="text-align:left;">

Hungarian National Authority for Data Protection and the Freedom of
Information (NAIH)

</td>

<td style="text-align:left;">

10/28/2019

</td>

<td style="text-align:left;">

Military Hospital

</td>

<td style="text-align:left;">

Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.naih.hu/files/NAIH-2019-2485-hatarozat.pdf>

</td>

<td style="text-align:left;">

A military hospital did not meet the reporting deadline imposed for data
breaches. The data protection authority further increased the fine due
to the lack of technical and organizational measured to prevent the loss
of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

155

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1600

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

12/10/2019

</td>

<td style="text-align:left;">

Megastar SL

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://cms.law/en/int/publication/data-law-navigator/spain>

</td>

<td style="text-align:left;">

The company was fined because it operated a video surveillance system
that had an observation angle that extended too far into the public
traffic area. The video surveillance system was also not accompanied by
any data protection notices.

</td>

</tr>

<tr>

<td style="text-align:right;">

156

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

12/03/2019

</td>

<td style="text-align:left;">

Cerrajeria Verin S.L.

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00265-2019_ORI.pdf>

</td>

<td style="text-align:left;">

The company was fined because it collected personal data without
providing accurate information about its data processing activities on
their privacy policy page on their website.

</td>

</tr>

<tr>

<td style="text-align:right;">

157

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

12/03/2019

</td>

<td style="text-align:left;">

Linea Directa Aseguradora

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00250-2019_ORI.pdf>

</td>

<td style="text-align:left;">

An insurance company sent advertising emails to clients without the
necessary consent.

</td>

</tr>

<tr>

<td style="text-align:right;">

158

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/19/2019

</td>

<td style="text-align:left;">

Sports Bar

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00236-2019_ORI.pdf>

</td>

<td style="text-align:left;">

A sports bar operated a video surveillance CCTV system that had an angle
that extended into a public area that should not have been surveilled.

</td>

</tr>

<tr>

<td style="text-align:right;">

159

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

11/06/2019

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00140-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Vodafone sent customers invoicing details to a third party after a
customer invoicing complaint. The fine was originally determined to be
€75,000 but later reduced to €60,000 after the quick cooperation of
the company.

</td>

</tr>

<tr>

<td style="text-align:right;">

160

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

10/23/2019

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/resoluciones/PS-00249-2019_ORI.pdf>

</td>

<td style="text-align:left;">

Invoicing details of a customer were sent to a third party customer
during an invoicing complaint.

</td>

</tr>

<tr>

<td style="text-align:right;">

161

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

12/10/2019

</td>

<td style="text-align:left;">

Shop Macoyn, S.L.

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00320-2019.pdf>

</td>

<td style="text-align:left;">

The company sent advertising emails to multiple recipients where every
one of the recipients was able to see the email address of all other
recipients. This was because the sender sent all the email addresses as
CC instead of BCC.

</td>

</tr>

<tr>

<td style="text-align:right;">

162

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

12/18/2019

</td>

<td style="text-align:left;">

Telekom Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=O_noua_amenda_pentru_incalcarea_RGPD_comunicat_decembrie&lang=ro>

</td>

<td style="text-align:left;">

The company did not ensure the accuracy of the processing of personal
data. This resulted in the disclosure of a client&\#8217;s personal data
to a different client.

</td>

</tr>

<tr>

<td style="text-align:right;">

163

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

12/16/2019

</td>

<td style="text-align:left;">

Globus Score SRL

</td>

<td style="text-align:left;">

Art. 58 GDPR

</td>

<td style="text-align:left;">

Non-cooperation with Data Protection Authority

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Alta_amenda_pentru_incalcarea_GDPR&lang=en>

</td>

<td style="text-align:left;">

The company did not comply with measures imposed by the Data Protection
Authority.

</td>

</tr>

<tr>

<td style="text-align:right;">

164

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

12/02/2019

</td>

<td style="text-align:left;">

Nicola Medical Team 17 SRL

</td>

<td style="text-align:left;">

Art. 58 GDPR

</td>

<td style="text-align:left;">

Non-cooperation with Data Protection Authority

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=Noi_amenzi_in_aplicarea_RGPD&lang=ro>

</td>

<td style="text-align:left;">

The company did not comply with measures imposed by the Data Protection
Authority.

</td>

</tr>

<tr>

<td style="text-align:right;">

165

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

11/29/2019

</td>

<td style="text-align:left;">

Homeowners Association

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Amenda_asociatie_proprietari&lang=ro>

</td>

<td style="text-align:left;">

A homeowners association used video surveillance CCTV without the proper
information displayed in the affected areas. The Data Protection
Authority also determined that inadequate technical measures were in
place regarding the access and storage of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

166

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

11/26/2019

</td>

<td style="text-align:left;">

Modern Barber

</td>

<td style="text-align:left;">

Art. 58 GDPR

</td>

<td style="text-align:left;">

Non-cooperation with Data Protection Authority

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=Noi_amenzi_in_aplicarea_RGPD&lang=ro>

</td>

<td style="text-align:left;">

The company did not comply with measures imposed by the Data Protection
Authority.

</td>

</tr>

<tr>

<td style="text-align:right;">

167

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/united-kingdom.svg>

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

320000

</td>

<td style="text-align:left;">

Information Commissioner (ICO)

</td>

<td style="text-align:left;">

12/20/2019

</td>

<td style="text-align:left;">

Doorstep Dispensaree

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://ico.org.uk/media/action-weve-taken/enforcement-notices/2616741/doorstop-en-20191217.pdf>

</td>

<td style="text-align:left;">

The company had stored some 500,000 documents containing names,
addresses, dates of birth, NHS numbers and medical information and
prescriptions in unsealed containers at the back of the building and
failed to protect these documents from the elements, resulting in water
damage to the documents.The company stored around 500,000 documents that
contained the names, addresses, birth fates, and NHS identification
numbers as well as medical information and prescriptions in unsealed
containers at the back of a building. As a result of this, the documents
were exposed to the elements which resulted in water damage and
potentially to the loss of some data.

</td>

</tr>

<tr>

<td style="text-align:right;">

168

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/netherlands.svg>

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Dutch Supervisory Authority for Data Protection (AP)

</td>

<td style="text-align:left;">

10/31/2019

</td>

<td style="text-align:left;">

Menzis

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://autoriteitpersoonsgegevens.nl/nl/nieuws/sancties-voor-menzis-en-vgz-voor-overtreding-van-de-privacywet>

</td>

<td style="text-align:left;">

The marketing staff of the health insurance company Menzis had access to
patients&\#8217; data.

</td>

</tr>

<tr>

<td style="text-align:right;">

169

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/greece.svg>

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Hellenic Data Protection Authority (HDPA)

</td>

<td style="text-align:left;">

10/18/2019

</td>

<td style="text-align:left;">

Wind Hellas Telecommunications

</td>

<td style="text-align:left;">

Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<http://www.dpa.gr/APDPXPortlets/htdocs/documentDisplay.jsp?docid=146,94,80,247,188,211,182,68>

</td>

<td style="text-align:left;">

The company ignored objections voiced by the affected parties regarding
advertising and marketing calls.

</td>

</tr>

<tr>

<td style="text-align:right;">

170

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/sweden.svg>

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:right;">

35000

</td>

<td style="text-align:left;">

Data Protection Authority of Sweden

</td>

<td style="text-align:left;">

12/16/2019

</td>

<td style="text-align:left;">

Nusvar AB

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.datainspektionen.se/globalassets/dokument/beslut/beslut-tillsyn-mrkoll.pdf>

</td>

<td style="text-align:left;">

Nusvar AB, which operates the website Mrkoll.se, a site that provides
information on all Swedes over the age of 16, published information on
people with overdue payments.

</td>

</tr>

<tr>

<td style="text-align:right;">

171

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/belgium.svg>

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Belgian Data Protection Authority (APD)

</td>

<td style="text-align:left;">

12/17/2019

</td>

<td style="text-align:left;">

Legal information wesbite

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 12 GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.gegevensbeschermingsautoriteit.be/sites/privacycommission/files/documents/BETG_12-2019_NL.PDF>

</td>

<td style="text-align:left;">

A website that provided legal information and news only had its privacy
policy page available in English, even though it was also addressing the
French and Dutch-speaking markets. Also, the privacy policy page was not
easily accessible and did not mention the legal basis for the processing
of data, as required by the GDPR. The website also used Google Analytics
without effective consent.

</td>

</tr>

<tr>

<td style="text-align:right;">

172

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

28100

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

09/03/2019

</td>

<td style="text-align:left;">

National Revenue Agency

</td>

<td style="text-align:left;">

Art 6 (1) GDPR|Art 58 (2) e) GDPR|Art 83 (5) a) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/?p=element_view&aid=2226>

</td>

<td style="text-align:left;">

The National Revenue Agency was fined with €28,100 because of the
unlawful processing of personal data of a private individual. The
personal data of the individual was unlawfully collected and used in an
enforcement case against them in order to recover a tax debt of €86,000.
The National Revenue Agency also collected bank account data of the
affected individual from the Bulgarian National Bank. The Bulgarian DPA
argued that this data was collected unlawfully by the National Revenue
Agency. This is one of the very rare cases where a DPA fines a
government institution for the unlawful processing of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

173

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

11760

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

09/03/2019

</td>

<td style="text-align:left;">

Commercial representative of telecommunication service provider

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=element&aid=1219>

</td>

<td style="text-align:left;">

A fine of €11,760 was issued on the commercial representative of a
national telecommunications provider due to the unlawful processing of
the personal data of a client. The commercial representative unlawfully
processed the data of a client with the goal of closing a contract for
mobile telephoning services.

</td>

</tr>

<tr>

<td style="text-align:right;">

174

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

5113

</td>

<td style="text-align:left;">

Bulgarian Commission for Personal Data Protection (KZLD)

</td>

<td style="text-align:left;">

09/03/2019

</td>

<td style="text-align:left;">

Telecommunication service provider

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=element&aid=1219>

</td>

<td style="text-align:left;">

A fine of €5,113 was imposed on a national telecom company for the
unlawful processing of the personal data of a citizen. The personal data
of the individual was unlawfully accessed and processed in order to
cancel a contract.

</td>

</tr>

<tr>

<td style="text-align:right;">

175

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

5112

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

10/08/2019

</td>

<td style="text-align:left;">

Interior Ministry

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=element&aid=1219>

</td>

<td style="text-align:left;">

The Bulgarian Interior Ministry was fined due to the unlawful processing
of the personal data of a citizen. The Interior Ministry unlawfully sent
the personal data of the citizen to the Togolese Republic.

</td>

</tr>

<tr>

<td style="text-align:right;">

176

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1121

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

09/03/2019

</td>

<td style="text-align:left;">

Private enforcement agent

</td>

<td style="text-align:left;">

Art. 12 (4) GDPR|Art. 15 GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=element&aid=1219>

</td>

<td style="text-align:left;">

A private enforcement agent was fined for the unlawful processing of
personal data of an individual. The agent had conducted video
surveillance on the individual and refused to grant access to the
collected data. The affected individual submitted an application to
access their personal data but the enforcement agent rejected this
request and refused to motivate this decision.

</td>

</tr>

<tr>

<td style="text-align:right;">

177

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1022

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

09/03/2019

</td>

<td style="text-align:left;">

Telecom company

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR|Art. 25 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=element&aid=1219>

</td>

<td style="text-align:left;">

A fine of €1,022 was imposed on a national telecom company for the
unlawful processing of the personal data of a citizen. The personal data
of the individual was unlawfully accessed and processed in order to
cancel a contract.

</td>

</tr>

<tr>

<td style="text-align:right;">

178

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

511

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

10/28/2019

</td>

<td style="text-align:left;">

Employer

</td>

<td style="text-align:left;">

Art. 12 (3) GDPR|Art. 15 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with subjects’ rights protection safeguards

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=element&aid=1219>

</td>

<td style="text-align:left;">

A company was fined with €511 because it refused to give access to the
personal data of an employee who submitted an application to receive
access to their personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

179

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/bulgaria.svg>

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

511

</td>

<td style="text-align:left;">

Data Protection Commission of Bulgaria (KZLD)

</td>

<td style="text-align:left;">

10/07/2019

</td>

<td style="text-align:left;">

BD

</td>

<td style="text-align:left;">

Art. 31 GDPR

</td>

<td style="text-align:left;">

Lack of cooperation with Data Protection Authority

</td>

<td style="text-align:left;">

<https://www.cpdp.bg/index.php?p=element&aid=1219>

</td>

<td style="text-align:left;">

BD was fined with €511 because it failed to provide access to
information which the national DPA requested in order to resolve a
complaint.

</td>

</tr>

<tr>

<td style="text-align:right;">

180

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

800

</td>

<td style="text-align:left;">

Data Protection Authority of Mecklenburg-Vorpommern

</td>

<td style="text-align:left;">

08/06/2019

</td>

<td style="text-align:left;">

Police Officer

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.faz.net/aktuell/gesellschaft/kriminalitaet/polizisten-nutzten-daten-um-minderjaehrige-maedchen-zu-kontaktieren-16227841.html>

</td>

<td style="text-align:left;">

A police officer used a witnesse&\#8217;s personal data to contact her.

</td>

</tr>

<tr>

<td style="text-align:right;">

181

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

118

</td>

<td style="text-align:left;">

Data Protection Authority of Saarland

</td>

<td style="text-align:left;">

01/01/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://indd.adobe.com/view/d639298c-3165-4e30-85d8-0730de2a3598>

</td>

<td style="text-align:left;">

Personal data was disclosed to a third party. No further details were
revealed.

</td>

</tr>

<tr>

<td style="text-align:right;">

182

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

294000

</td>

<td style="text-align:left;">

Data Protection Authority of Niedersachsen

</td>

<td style="text-align:left;">

12/02/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.handelsblatt.com/politik/deutschland/dsgvo-datenschutz-verstoesse-zahl-der-bussgelder-ist-drastisch-gestiegen/25364576.html?ticket=ST-1616678-HaaxMJeMxarATmdXeL7Z-ap6>

</td>

<td style="text-align:left;">

A company was fined with €294,000 because of the &\#8220;unnecessarily
long&\#8221; storage and retention of personal data in the selection of
personnel. During the selection process, even health data was requested,
which was excessive according to the DPA.

</td>

</tr>

<tr>

<td style="text-align:right;">

183

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

01/13/2020

</td>

<td style="text-align:left;">

Government agency

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<http://www.dataprotection.gov.cy/dataprotection/dataprotection.nsf/All/ACDFDC478581BEE1C22584EE002EE9C2?OpenDocument>

</td>

<td style="text-align:left;">

The government agency was fined due to granting the police access to
data and failing to implement adequate measures to secure the data, even
after being warned by the national DPA.

</td>

</tr>

<tr>

<td style="text-align:right;">

184

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

1000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

01/13/2020

</td>

<td style="text-align:left;">

M.L. Pro.Fit Solutions LTD

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<http://www.dataprotection.gov.cy/dataprotection/dataprotection.nsf/All/ACDFDC478581BEE1C22584EE002EE9C2?OpenDocument>

</td>

<td style="text-align:left;">

The company was fined for sending marketing messages without consent. No
measures were taken to permit phone users to block these messages and no
measures were provided that would have allowed the contacted individuals
to opt-out of receiving these messages.

</td>

</tr>

<tr>

<td style="text-align:right;">

185

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

70000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

10/25/2019

</td>

<td style="text-align:left;">

LGS Handling Ltd

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<http://www.dataprotection.gov.cy/dataprotection/dataprotection.nsf/all/ACDFDC478581BEE1C22584EE002EE9C2/$file/2019-apofasi%20bradford%20system%20%CE%91%CE%9D%CE%A9%CE%9D%CE%A5%CE%9C%CE%9F%CE%A0.pdf?openelement>

</td>

<td style="text-align:left;">

The national data protection authority determined that the company used
the Bradford factor for profiling and monitoring sick leave and that
this constituted unlawful processing of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

186

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

10/25/2019

</td>

<td style="text-align:left;">

Louis Travel Ltd

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<http://www.dataprotection.gov.cy/dataprotection/dataprotection.nsf/all/ACDFDC478581BEE1C22584EE002EE9C2/$file/2019-apofasi%20bradford%20system%20%CE%91%CE%9D%CE%A9%CE%9D%CE%A5%CE%9C%CE%9F%CE%A0.pdf?openelement>

</td>

<td style="text-align:left;">

The national data protection authority determined that the company used
the Bradford factor for profiling and monitoring sick leave and that
this constituted unlawful processing of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

187

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/cyprus.svg>

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Cypriot Data Protection Commissioner

</td>

<td style="text-align:left;">

10/25/2019

</td>

<td style="text-align:left;">

Louis Aviation Ltd

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<http://www.dataprotection.gov.cy/dataprotection/dataprotection.nsf/all/ACDFDC478581BEE1C22584EE002EE9C2/$file/2019-apofasi%20bradford%20system%20%CE%91%CE%9D%CE%A9%CE%9D%CE%A5%CE%9C%CE%9F%CE%A0.pdf?openelement>

</td>

<td style="text-align:left;">

The national data protection authority determined that the company used
the Bradford factor for profiling and monitoring sick leave and that
this constituted unlawful processing of personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

188

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/greece.svg>

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Hellenic Data Protection Authority (HDPA)

</td>

<td style="text-align:left;">

01/13/2020

</td>

<td style="text-align:left;">

Allseas Marine S.A.

</td>

<td style="text-align:left;">

Art. 5 (1) a), (2) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<http://www.dpa.gr/APDPXPortlets/htdocs/documentDisplay.jsp?docid=126,92,211,86,111,236,222,151>

</td>

<td style="text-align:left;">

The company unlawfully introduced a video surveillance system at the
workplace to monitor employee activity. The Hellenic Data Protection
Authority (HDPA) argued that the installation of the system was unlawful
because the employees were not notified of the existence of the system.

</td>

</tr>

<tr>

<td style="text-align:right;">

189

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/italy.svg>

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

8500000

</td>

<td style="text-align:left;">

Italian Data Protection Authority (Garante)

</td>

<td style="text-align:left;">

01/17/2020

</td>

<td style="text-align:left;">

Eni Gas e Luce

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 17 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.gpdp.it/web/guest/home/docweb/-/docweb-display/docweb/9244365>

</td>

<td style="text-align:left;">

The Italian Data Protection Authority (Garante) imposed two fines of
€11,5 million total on Eni Gas and Luce because of the unlawful
processing of personal data during an advertising campaign as well as
for the activation of unsolicited contracts. This first fine of €8,5
million was issued for the unlawful processing of personal data in the
context of a marketing campaign. The company made promotional calls
without the consent of the contacted people and refused to acknowledge
people&\#8217;s wishes to be added onto a &\#8220;do not contact&\#8221;
list. The company also did not provide an opt-out procedure for these
unsolicited calls. The DPA also determined that the company lacked
sufficient technical and organizational measures to protect
users&\#8217; personal data. Data was also processed longer than the
allowed retention period. According to the DPA, some data was also
collected from third party entities that did not have consent from the
data subjects to disclose that data.

</td>

</tr>

<tr>

<td style="text-align:right;">

190

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/italy.svg>

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

3000000

</td>

<td style="text-align:left;">

Italian Data Protection Authority (Garante)

</td>

<td style="text-align:left;">

01/17/2020

</td>

<td style="text-align:left;">

Eni Gas e Luce

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.gpdp.it/web/guest/home/docweb/-/docweb-display/docweb/9244365>

</td>

<td style="text-align:left;">

The Italian Data Protection Authority (Garante) imposed two fines of
€11,5 million total on Eni Gas and Luce because of the unlawful
processing of personal data during an advertising campaign as well as
for the activation of unsolicited contracts. This second fine of €3
million was issued for the opening of unsolicited contracts for the
provision of electricity and gas. A large number of individuals have
reported that they have only learned of the new contracts after they
received a termination letter from their old provider. Some complaints
even reported false data as well as forged signatures.

</td>

</tr>

<tr>

<td style="text-align:right;">

191

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

01/14/2020

</td>

<td style="text-align:left;">

SC Enel Energie S.A.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 7 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=sanctiune_pentru_incalcarea_RGPD_2020_2&lang=ro>

</td>

<td style="text-align:left;">

The fine was issued after a complaint alleging that Enel Energie had
processed an individual&\#8217;s personal data and that the natural gas
and electricity company was unable to prove it obtained the
individual&\#8217;s consent to send email notifications. The national
data protection authority also explained that the company had not taken
the required measures to stop the transmission of the email
notifications even after the affected person had made a request to this
end. The company was fined two times €3,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

192

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

12/13/2019

</td>

<td style="text-align:left;">

Entirely Shipping & Trading S.R.L.

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 6 GDPR|Art. 7 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=O_noua_sanctiune_pentru_incalcarea_RGPD_2020_3&lang=ro>

</td>

<td style="text-align:left;">

The company installed video surveillance in order to monitor employee
activity. The problem arose from the fact that some cameras were
installed in the locker rooms where the staff kept their spare clothes
and regularly used to get dressed and undressed.

</td>

</tr>

<tr>

<td style="text-align:right;">

193

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

12/13/2019

</td>

<td style="text-align:left;">

Entirely Shipping & Trading S.R.L.

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 6 GDPR|Art. 7 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=O_noua_sanctiune_pentru_incalcarea_RGPD_2020_3&lang=ro>

</td>

<td style="text-align:left;">

A second fine was issued to the company for the unlawful processing of
employee biometric data (fingerprints). The processing of biometric data
allegedly was necessary to give employees access to certain rooms. The
national DPA argued that this was too excessive.

</td>

</tr>

<tr>

<td style="text-align:right;">

194

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

12/10/2019

</td>

<td style="text-align:left;">

Hora Credit IFN SA

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 25 GDPR|Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=Alta_amenda_pentru_incalcarea_RGPD_2020_1&lang=ro>

</td>

<td style="text-align:left;">

Three fined were issued on Hora Credit IFN SA because personal data of
an individual was transmitted through email to a third party. The
following investigation revealed that the company processed personal
data without any means to validate the accuracy and authenticity of the
data collected and processed. The operator also did not employ enough
technical and organizational measures to protect the collected personal
data. The case was made worse by the fact that the company did not
notify the ANSPDCP after the data breach was discovered, as required by
the law. The three fined issued were of €3,000, €10,000 and €1,000 for
all the three issues of non-compliance discovered by the ANSPDCP.

</td>

</tr>

<tr>

<td style="text-align:right;">

195

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/09/2020

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 58 GDPR

</td>

<td style="text-align:left;">

Non-cooperation with Data Protection Authority

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00445-2019.pdf>

</td>

<td style="text-align:left;">

The company did not provide information to the AEPD in relation to an
investigation.

</td>

</tr>

<tr>

<td style="text-align:right;">

196

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

44000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/07/2020

</td>

<td style="text-align:left;">

Vodafone Espana

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00093-2019.pdf>

</td>

<td style="text-align:left;">

The company was fined because it sent a contract that included the name
and address and contact details of a client to a third party by
accident.

</td>

</tr>

<tr>

<td style="text-align:right;">

197

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/07/2020

</td>

<td style="text-align:left;">

EDP España S.A.U.

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00109-2019.pdf>

</td>

<td style="text-align:left;">

The company was fined because it processed personal data such as name,
tax identification number, address and phone number without the consent
of the affected individuals.

</td>

</tr>

<tr>

<td style="text-align:right;">

198

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/07/2020

</td>

<td style="text-align:left;">

EDP Comercializadora, S.A.U.

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00025-2019.pdf>

</td>

<td style="text-align:left;">

The company was fined because it processed personal data in regards to a
gas contract without the applicants&\#8217; consent. The investigation
revealed that the applicant received an invoice for the supplying of
natural gas, a contract which they didn&\#8217;t sign. EDP
Comercializadora argued that since the applicant had a contract with
another gas company with which EDP Comercializadora had a collaboration
agreement, it was justified to process the personal data of the
respective individuals. The AEPD, however, ruled that the company was
required to receive permission directly from the affected individuals to
process personal data.

</td>

</tr>

<tr>

<td style="text-align:right;">

199

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/07/2020

</td>

<td style="text-align:left;">

Asociación de Médicos Demócratas

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00231-2019.pdf>

</td>

<td style="text-align:left;">

The organization processed personal data of its members even after the
AEPD warned it that the processing was unlawful without the consent of
the affected individuals.

</td>

</tr>

<tr>

<td style="text-align:right;">

200

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/italy.svg>

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

27802946

</td>

<td style="text-align:left;">

Italian Data Protection Authority (Garante)

</td>

<td style="text-align:left;">

02/01/2020

</td>

<td style="text-align:left;">

TIM - Telecom Provider

</td>

<td style="text-align:left;">

Art. 58(2) GDPR

</td>

<td style="text-align:left;">

Non-cooperation with Data Protection Authority

</td>

<td style="text-align:left;">

<https://www.garanteprivacy.it/web/guest/home/docweb/-/docweb-display/docweb/9256409>

</td>

<td style="text-align:left;">

A huge fine of €27,8 million was issued to the Italian telecom company
TIM. The Italian Data Protection Authority (Garante) revealed that TIM
was fined due to numerous unlawful data processing activities related to
marketing and advertising, which included unsolicited promotional calls
and prize competitions in which data subjects were entered without
consent.One of the reasons for the large fine was the fact that the
unlawful data processing activities involved several million
individuals. One individual, for example, was called a total of 155
times in a month while TIM refused to add the affected individual on a
no-call list even after several requests. The DPA determined that the
company lacked control over the call centers and did not have adequate
measures to add people to no-call lists.TIM also did not provide
accurate and detailed enough privacy policies and data processing
policies, and as such consumers were not efficiently informed about the
data collected and processed. The company&\#8217;s management of data
breaches was also not efficient according to Garante.Besides the fine,
Garante also imposed 20 corrective measures according to Art. 58(2) GDPR
which prohibits TIM from processing marketing-related data of those
individuals who have refused to receive promotional calls, individuals
who asked to be blacklisted and individuals who are not clients of
TIM.The company was also forbidden from using customer data collected
from the &\#8220;My Tim&\#8221;, &\#8220;Tim Personal&\#8221; and
&\#8220;Tim Smart Kid&\#8221; apps.

</td>

</tr>

<tr>

<td style="text-align:right;">

201

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Xfera Moviles S.A.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00227-2019.pdf>

</td>

<td style="text-align:left;">

The Spanish Data Protection Authority revealed that Xfera Moviles S.A.
has unlawfully processed data that included bank details, customer
address as well as name of various individuals.

</td>

</tr>

<tr>

<td style="text-align:right;">

202

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Vodafone España, S.A.U.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00270-2019.pdf>

</td>

<td style="text-align:left;">

Vodafone España has signed a contract regarding the transfer of a phone
subscription with a third party person without the account
holder&\#8217;s knowledge or permission. The account holder received an
email from the third party regarding the purchase that was made in his
name.

</td>

</tr>

<tr>

<td style="text-align:right;">

203

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Vodafone España, S.A.U.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00405-2019.pdf>

</td>

<td style="text-align:left;">

A customer complained that the company had processed their personal data
without their consent because an email was sent to them on behalf of a
company regarding the purchase of a service that was actually not bought
by the respective individual. The personal details of the individuals
were incorporated into Vodafone España&\#8217;s systems without the
consent of that individual. Initially, the fine was determined to be
€100,000 but was reduced to €60,000.

</td>

</tr>

<tr>

<td style="text-align:right;">

204

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Vodafone España, S.A.U.

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00275-2019.pdf>

</td>

<td style="text-align:left;">

Vodafone España sent invoices of a client that contained personal data
such as name, ID card number, and address to their neighbor.

</td>

</tr>

<tr>

<td style="text-align:right;">

205

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Iberia Lineas Aereas

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00402-2019.pdf>

</td>

<td style="text-align:left;">

The company continued to send emails to individuals even after the
affected individuals have requested to be removed from the
company&\#8217;s database or be added to a &\#8220;no-contact&\#8221;
list.

</td>

</tr>

<tr>

<td style="text-align:right;">

206

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Vodafone España, S.A.U.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00278-2019.pdf>

</td>

<td style="text-align:left;">

A former customer of the company continued to receive invoice notices
even after the contractual obligation between the two parties has ended.
The company indicated a technical error for the issuing of the
unsolicited notices.

</td>

</tr>

<tr>

<td style="text-align:right;">

207

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

6670

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Banco Bilbao

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00400-2019.pdf>

</td>

<td style="text-align:left;">

The company has sent several advertising messages to a person, even
after the affected person made it clear that they do not consent to
their personal data to be processed.

</td>

</tr>

<tr>

<td style="text-align:right;">

208

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Queseria Artesenal Ameco S.L.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00259-2019.pdf>

</td>

<td style="text-align:left;">

The company was fined because it processed personal data without the
consent of the affected parties.

</td>

</tr>

<tr>

<td style="text-align:right;">

209

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

800

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/03/2020

</td>

<td style="text-align:left;">

Private person

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00292-2019.pdf>

</td>

<td style="text-align:left;">

A private individual created a fake profile of a female colleague on an
erotic website. The profile contained the affected person&\#8217;s
contact details and pictures as well as information of sexual nature.
The affected person then received several phone calls from several
people who were inquiring about the fake profile. The person who created
the fake profile was found to have a personality disorder and as such
the fine was reduced from €1,000 to €800.

</td>

</tr>

<tr>

<td style="text-align:right;">

210

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/04/2020

</td>

<td style="text-align:left;">

Cafetería Nagasaki

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00427-2018.pdf>

</td>

<td style="text-align:left;">

According to the AEPD, Cafetería Nagasaki did not comply with its
obligations under the GDPR because it installed surveillance cameras in
such a way that it also monitored the public space outside of the
restaurant which also captured pedestrians on the street.

</td>

</tr>

<tr>

<td style="text-align:right;">

211

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

3600

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

01/14/2020

</td>

<td style="text-align:left;">

Zhang Bordeta 2006, S.L.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00397-2019.pdf>

</td>

<td style="text-align:left;">

The store was fined because it installed video surveillance that also
took images of the sidewalk in front of it and invaded
pedestrians&\#8217; privacy.

</td>

</tr>

<tr>

<td style="text-align:right;">

212

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/italy.svg>

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Italian Data Protection Authority (Garante)

</td>

<td style="text-align:left;">

01/15/2020

</td>

<td style="text-align:left;">

Francavilla Fontana

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.gpdp.it/web/guest/home/docweb/-/docweb-display/docweb/9261227>

</td>

<td style="text-align:left;">

The local community of Francavilla Fontana published online the details
of an ongoing court trial that included personal information such as
health data of several individuals.

</td>

</tr>

<tr>

<td style="text-align:right;">

213

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

51000

</td>

<td style="text-align:left;">

Data Protection Authority of Hamburg

</td>

<td style="text-align:left;">

01/01/2019

</td>

<td style="text-align:left;">

Facebook

</td>

<td style="text-align:left;">

Art. 37 GDPR

</td>

<td style="text-align:left;">

Failure to appoint a data protection officer

</td>

<td style="text-align:left;">

<https://datenschutz-hamburg.de/assets/pdf/28._Taetigkeitsbericht_Datenschutz_2019_HmbBfDI.pdf>

</td>

<td style="text-align:left;">

The German branch of Facebook was fined by €51,000 because it failed to
appoint a data protection officer. Facebook argued that it did in fact
appoint a data protection officer in Ireland who acted as a data
protection officer for all the local European Facebook branches. The
Data Protection Authority of Hamburg, however, argued that Facebook did
not notify the German authority about this appointment, and as such, the
fine is valid. The reason the fine was relatively small was that
Facebook did, after all, appoint a DPO but failed to notify German
authorities. The fine was given to Facebook Germany GmbH, which is the
local German branch of the company.The fine was issued sometimes in 2019
but was only made public by the Data Protection Authority of Hamburg in
February 2020. The exact date of the fine was not revealed.

</td>

</tr>

<tr>

<td style="text-align:right;">

214

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/28/2020

</td>

<td style="text-align:left;">

Vodafone ONO

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://platform.dataguidance.com/sites/default/files/ps-00212-2019.pdf>

</td>

<td style="text-align:left;">

The company was fined due to several deficiencies in information
security. Two clients of the company had received the same security
access key, allowing to view each others&\#8217; personal details.

</td>

</tr>

<tr>

<td style="text-align:right;">

215

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

120000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/27/2020

</td>

<td style="text-align:left;">

Vodafone España

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://platform.dataguidance.com/sites/default/files/ps-00235-2019.pdf>

</td>

<td style="text-align:left;">

The company was not able to prove that an individual had given them
consent to access and process their personal data with the goal of
opening a telephone contract. The AEPD further explained that the
company unlawfully disclosed the affected person&\#8217;s personal data
to third party credit agencies.

</td>

</tr>

<tr>

<td style="text-align:right;">

216

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/25/2020

</td>

<td style="text-align:left;">

HM Hospitales

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://platform.dataguidance.com/sites/default/files/ps-00187-2019.pdf>

</td>

<td style="text-align:left;">

An individual reported that at the time of their admission to the
hospital they had to fill in a form that had a checkbox that indicated
that if the checkbox is not ticked, the hospital can transfer the
person&\#8217;s private data to third parties. The data protection
authority argued that this form was not in accordance with the GDPR
because consent was to be obtained from the inactivity of the affected
person.

</td>

</tr>

<tr>

<td style="text-align:right;">

217

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/18/2020

</td>

<td style="text-align:left;">

Mymoviles Europa 2000

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Insufficient fulfilment of information obligations

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00423-2019.pdf>

</td>

<td style="text-align:left;">

The Spanish Data Protection Authority determined that the company did
not publish a privacy statement on its website and the short legal
notice that was posted was not enough to properly identify the company
and explain its data processing policies.

</td>

</tr>

<tr>

<td style="text-align:right;">

218

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/14/2020

</td>

<td style="text-align:left;">

Grupo Valsor Y Losan

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00298-2019.pdf>

</td>

<td style="text-align:left;">

The company had disclosed the third party data of a client during a
property purchase agreement.

</td>

</tr>

<tr>

<td style="text-align:right;">

219

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/14/2020

</td>

<td style="text-align:left;">

Colegio Arenales Carabanchel

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00466-2019.pdf>

</td>

<td style="text-align:left;">

The Spanish Data Protection Authority explained that the school had
transferred pictures of students to third parties who then posted those
pictures online.

</td>

</tr>

<tr>

<td style="text-align:right;">

220

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/14/2020

</td>

<td style="text-align:left;">

Iberdrola Clientes

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00181-2019.pdf>

</td>

<td style="text-align:left;">

The electricity company Iberdrola Clientes closed a client&\#8217;s
contract without their consent and opened three new contracts on their
name also without their consent. The company also transferred the
client&\#8217;s personal data to third-party entities without a legal
basis.

</td>

</tr>

<tr>

<td style="text-align:right;">

221

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

42000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/14/2020

</td>

<td style="text-align:left;">

Vodafone España

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00471-2019.pdf>

</td>

<td style="text-align:left;">

An individual reported having had access to the personal data to third
parties in their personal Vodafone profile.

</td>

</tr>

<tr>

<td style="text-align:right;">

222

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

02/14/2020

</td>

<td style="text-align:left;">

Xfera Moviles S.A.

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00385-2019.pdf>

</td>

<td style="text-align:left;">

The Spanish Data Protection Authority determined that a customer of the
company had access to the personal data of other customers.

</td>

</tr>

<tr>

<td style="text-align:right;">

223

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

24000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

03/03/2020

</td>

<td style="text-align:left;">

Vodafone España

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00426-2019.pdf>

</td>

<td style="text-align:left;">

The company sent two SMS messages to a person informing them about the
rate change of a contract as well as the purchase of a mobile phone. The
customer did not consent to the processing of their personal data and
Vodafone sent the text messages without prior written consent from the
customer.

</td>

</tr>

<tr>

<td style="text-align:right;">

224

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

03/03/2020

</td>

<td style="text-align:left;">

Vodafone España

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00421-2019.pdf>

</td>

<td style="text-align:left;">

The company sent a text message to a person&\#8217;s phone number
informing them that their contract was modified. The affected person,
however, was not actually a Vodafone client. The AEPD determined that
Vodafone had processed the affected person&\#8217;s personal details
without consent.

</td>

</tr>

<tr>

<td style="text-align:right;">

225

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

42000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

03/03/2020

</td>

<td style="text-align:left;">

Vodafone España

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00474-2019.pdf>

</td>

<td style="text-align:left;">

A client&\#8217;s personal data was accessed without authorization. The
AEPD explained that this happened due to lack of technical and
organizational measures taken by the company to ensure information
security.

</td>

</tr>

<tr>

<td style="text-align:right;">

226

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1800

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

03/03/2020

</td>

<td style="text-align:left;">

Solo Embrague

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Insufficient fulfilment of information obligations

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00469-2019.pdf>

</td>

<td style="text-align:left;">

The website of the company did not contain a privacy policy or a cookie
banner.

</td>

</tr>

<tr>

<td style="text-align:right;">

227

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/netherlands.svg>

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

525000

</td>

<td style="text-align:left;">

Dutch Supervisory Authority for Data Protection (AP)

</td>

<td style="text-align:left;">

03/03/2020

</td>

<td style="text-align:left;">

Royal Dutch Tennis Assoc.

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://autoriteitpersoonsgegevens.nl/sites/default/files/atoms/files/boetebesluit_knltb.pdf>

</td>

<td style="text-align:left;">

The Royal Dutch Tennis Association (&\#8220;KNLTB&\#8221;) was fined a
total of €525,000 for selling the personal data of more than 350,000 of
its members to sponsors. The sponsors have then contacted some of these
individuals by email and telephone for marketing purposes. Personal data
sold included the name, gender, and address of various individuals. No
consent was obtained from the affected individuals beforehand. The Royal
Dutch Tennis Association (&\#8220;KNLTB&\#8221;) argued that it had a
legitimate interest to sell this data, and as such did not commit a GDPR
breach. The Dutch Data Protection Authority, however, rejected this and
ruled that KNLTB had no legal basis to sell the personal data of its
members to third parties.

</td>

</tr>

<tr>

<td style="text-align:right;">

228

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/republic-of-poland.svg>

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

4600

</td>

<td style="text-align:left;">

Polish National Personal Data Protection Office (UODO)

</td>

<td style="text-align:left;">

03/04/2020

</td>

<td style="text-align:left;">

School in Gdansk

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://uodo.gov.pl/decyzje/ZSZZS.440.768.2018>

</td>

<td style="text-align:left;">

Biometric fingerprint scanners were used in a school in Gdansk (Poland)
to authenticate students into the school&\#8217;s payment processing
system. While the parents have given written consent to the processing
of this kind of data, the Polish National Personal Data Protection
Office (UODO) argued that the data processing was nevertheless unlawful,
as the consent was obtained involuntarily. It was argued that the school
required the consent, otherwise, it would not have been able to process
student&\#8217;s payments at all, meaning parents had no choice other
than to &\#8220;consent&\#8221;.

</td>

</tr>

<tr>

<td style="text-align:right;">

229

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Private person

</td>

<td style="text-align:left;">

03/06/2020

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00293-2019.pdf>

</td>

<td style="text-align:left;">

A private person unlawfully used CCTV cameras. The AEPD revealed that
the CCTC camera system used by the individual for home protection also
filmed part of a public space.

</td>

</tr>

<tr>

<td style="text-align:right;">

230

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/italy.svg>

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italian Data Protection Authority (Garante)

</td>

<td style="text-align:left;">

03/06/2020

</td>

<td style="text-align:left;">

Liceo Scientifico Nobel di Torre del Greco

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.garanteprivacy.it/web/guest/home/docweb/-/docweb-display/docweb/9283014>

</td>

<td style="text-align:left;">

The school unlawfully published health data of over 2,000 teachers on
its website.

</td>

</tr>

<tr>

<td style="text-align:right;">

231

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/italy.svg>

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italian Data Protection Authority (Garante)

</td>

<td style="text-align:left;">

03/06/2020

</td>

<td style="text-align:left;">

Liceo Artistico Statale di Napoli

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.garanteprivacy.it/web/guest/home/docweb/-/docweb-display/docweb/9283029>

</td>

<td style="text-align:left;">

The school unlawfully published health data of multiple teachers on its
website.

</td>

</tr>

<tr>

<td style="text-align:right;">

232

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/spain.svg>

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Spanish Data Protection Authority (AEPD)

</td>

<td style="text-align:left;">

03/09/2020

</td>

<td style="text-align:left;">

Gesthotel Activos Balagares

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.aepd.es/es/documento/ps-00358-2019.pdf>

</td>

<td style="text-align:left;">

The complainant explained that they had sent a letter to the management
of the hotel and union delegates that contained information related to
an episode of alleged harassment in relation to a medical condition. The
hotel management then read the contents of the letter in a meeting with
other employees. This constituted a violation of the principle of
integrity and confidentiality.

</td>

</tr>

<tr>

<td style="text-align:right;">

233

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/iceland.svg>

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Icelandic Data Protection Authority (‘Persónuvernd’)

</td>

<td style="text-align:left;">

03/10/2020

</td>

<td style="text-align:left;">

Breiðholt School

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.personuvernd.is/urlausnir/nr/2885>

</td>

<td style="text-align:left;">

A teacher had sent an email to parents and students that contained an
attachment that had detailed information on the well-being and academic
performance of all students.

</td>

</tr>

<tr>

<td style="text-align:right;">

234

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/iceland.svg>

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:right;">

20600

</td>

<td style="text-align:left;">

Icelandic Data Protection Authority (‘Persónuvernd’)

</td>

<td style="text-align:left;">

03/10/2020

</td>

<td style="text-align:left;">

Addiction Medicine Center

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.personuvernd.is/urlausnir/nr/2882>

</td>

<td style="text-align:left;">

A former employee of National Center of Addiction Medicine
(&\#8216;SAA&\#8217;) received boxes that contained personal belongings
that he supposedly left there but personal data and health records of
252 former patients and documents with the names of around 3,000
individuals who once participated in an alcohol and drug abuse
rehabilitation program.

</td>

</tr>

<tr>

<td style="text-align:right;">

235

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/denmark.svg>

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Danish Data Protection Authority (Datatilsynet)

</td>

<td style="text-align:left;">

03/10/2020

</td>

<td style="text-align:left;">

Gladsaxe Municipality

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.datatilsynet.dk/presse-og-nyheder/nyhedsarkiv/2020/mar/to-kommuner-indstillet-til-boede/>

</td>

<td style="text-align:left;">

A computer that belonged to the administration of the municipality was
stolen. The computer was not encrypted and it included the personal
identification numbers of 20,620 residents.

</td>

</tr>

<tr>

<td style="text-align:right;">

236

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/denmark.svg>

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

7000

</td>

<td style="text-align:left;">

Danish Data Protection Authority (Datatilsynet)

</td>

<td style="text-align:left;">

03/10/2020

</td>

<td style="text-align:left;">

Hørsholm Municipality

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.datatilsynet.dk/presse-og-nyheder/nyhedsarkiv/2020/mar/to-kommuner-indstillet-til-boede/>

</td>

<td style="text-align:left;">

A work computer belonging to a city government employee was stolen. The
computer contained personal data of around 1,600 city government
employees as well as sensitive information such as social security
numbers.

</td>

</tr>

<tr>

<td style="text-align:right;">

237

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/sweden.svg>

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:right;">

7000000

</td>

<td style="text-align:left;">

Data Protection Authority of Sweden

</td>

<td style="text-align:left;">

03/11/2020

</td>

<td style="text-align:left;">

Google

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 17 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.datainspektionen.se/globalassets/dokument/beslut/2020-03-11-beslut-google.pdf>

</td>

<td style="text-align:left;">

Google was fined with €7,000,000 by the Swedish Data Protection
Authority due to failing to adequately comply with its obligations
regarding the right of data subjects to have their search results
removed from Google search. The Data Protection Authority of Sweden had
already completed an investigation on Google in 2017 where it
investigated how the company dealt with individuals&\#8217; requests to
be removed from search results. At that time, the Data Protection
Authority instructed Google to be more pro-active in executing these
removal requests. In 2018 the Authority initialed a further
investigation after it was reported that Google did not remove search
results related to individuals even after the earlier instructions in
2017 to do so. The Authority also questioned Google&\#8217;s practice of
informing website owners about which search results Google had removed,
specifically which link (search result) has been removed and who was
behind the removal request.

</td>

</tr>

<tr>

<td style="text-align:right;">

238

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

02/11/2020

</td>

<td style="text-align:left;">

Vodafone Romania

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/?page=sanctiune_vodafone_februarie_2020&lang=ro>

</td>

<td style="text-align:left;">

Vodafone Romania employed weak and insufficient security measures, which
led to an event of erroneous data processing. When an individual issued
a complaint, the company incorrectly processed the client’s data and
sent it to a wrong e-mail address.

</td>

</tr>

<tr>

<td style="text-align:right;">

239

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

03/25/2020

</td>

<td style="text-align:left;">

eMag - Dante International

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=Comunicat_amenda_dante_international_martie_2020&lang=ro>

</td>

<td style="text-align:left;">

The company in question, Dante International, failed to observe the
right of a customer to unsubscribe from commercial communications and
sent a commercial e-mail to the client in spite of that.

</td>

</tr>

<tr>

<td style="text-align:right;">

240

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

4150

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

03/25/2020

</td>

<td style="text-align:left;">

Vodafone Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=Comunicat_noua_amenda_vodafone&lang=ro>

</td>

<td style="text-align:left;">

Vodafone Romania sent an e-mail containing personal data of a client to
another unrelated client, thus breaking privacy conventions. They had
improper organizational and security measures in effect at that time.

</td>

</tr>

<tr>

<td style="text-align:right;">

241

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

03/25/2020

</td>

<td style="text-align:left;">

Enel Energie

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=Comunicat_amenda_enel_martie_2020&lang=ro>

</td>

<td style="text-align:left;">

Enel Energie sent a client an email that contained the personal
information of another client, failing to employ the necessary
organizational and technical measures.

</td>

</tr>

<tr>

<td style="text-align:right;">

242

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/romania.svg>

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Romanian National Supervisory Authority for Personal Data Processing
(ANSPDCP)

</td>

<td style="text-align:left;">

03/25/2020

</td>

<td style="text-align:left;">

SOS Infertility Association

</td>

<td style="text-align:left;">

Art. 58 GDPR

</td>

<td style="text-align:left;">

Non-cooperation with Data Protection Authority

</td>

<td style="text-align:left;">

<https://www.dataprotection.ro/index.jsp?page=Comunicat_amenda_asociatia_sos_infertilitatea&lang=ro>

</td>

<td style="text-align:left;">

The SOS Infertility Association failed to provide the necessary data to
the data protection authority after it had unlawfully processed personal
data of its clients.

</td>

</tr>

<tr>

<td style="text-align:right;">

243

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/11/croatia.svg>

</td>

<td style="text-align:left;">

Croatia

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Croatian Data Protection Authority (AZOP)

</td>

<td style="text-align:left;">

03/13/2020

</td>

<td style="text-align:left;">

Bank (unknown)

</td>

<td style="text-align:left;">

Art. 15 (1), (3) GDPR

</td>

<td style="text-align:left;">

Information obligation non-compliance

</td>

<td style="text-align:left;">

<https://azop.hr/aktualno/detaljnije/rjesenje-kojim-se-izrice-upravno-novcana>

</td>

<td style="text-align:left;">

The bank did not provide its customers with copies of credit
documentation (interest changes reviews, repayment plans, and loan
agreement annexes) in the period from May 2018 to April 2019. In this
sense, the bank went ahead and argued that its decision was the right
one since the documentation would be related to repaid loans, which a
customer shouldn&\#8217;t have the right to access. A data subject
alerted the DPA, which demanded that the bank provide copies of the loan
documentation to the data subject. The DPA fined the bank (a specific
sum is still unknown) taking into consideration the financial
institution&\#8217;s continued refusal for over a year to deny the right
of access to such documentation to over 2.500 customers.

</td>

</tr>

<tr>

<td style="text-align:right;">

244

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/czech-republic.svg>

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Czech Data Protection Auhtority (UOOU)

</td>

<td style="text-align:left;">

03/21/2019

</td>

<td style="text-align:left;">

Unknown

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR

</td>

<td style="text-align:left;">

Non-compliance with lawful basis for data processing

</td>

<td style="text-align:left;">

<https://www.uoou.cz/assets/File.ashx?id_org=200144&id_dokumenty=34470>

</td>

<td style="text-align:left;">

The data was processed inadequately, in inobservance with the data
minimization and storage limitation principles of the GDPR. This means
the data that was processed went beyond the relevant needs for the
purpose of the processing, while also being kept in a form that permits
the identification of data subjects longer than it is necessary for the
purpose of the processing.

</td>

</tr>

<tr>

<td style="text-align:right;">

245

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Data Protection Authority of Saarland

</td>

<td style="text-align:left;">

01/01/2019

</td>

<td style="text-align:left;">

Restaurant

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.datenschutz.saarland.de/fileadmin/user_upload/uds/tberichte/tb28_2019.pdf>

</td>

<td style="text-align:left;">

The video surveillance cameras had been misused, clearly not in accord
with the data minimization principle.

</td>

</tr>

<tr>

<td style="text-align:right;">

246

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Data Protection Authority of Hamburg

</td>

<td style="text-align:left;">

01/01/2019

</td>

<td style="text-align:left;">

<https://datenschutz-hamburg.de/assets/pdf/28._Taetigkeitsbericht_Datenschutz_2019_HmbBfDI.pdf>

</td>

<td style="text-align:left;">

Art. 33 GDPR|Art. 34 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://datenschutz-hamburg.de/assets/pdf/28._Taetigkeitsbericht_Datenschutz_2019_HmbBfDI.pdf>

</td>

<td style="text-align:left;">

HVV GmbH had not reported a data breach to the data protection authority
in due time. This data breach was related to the security gap in the
Customer E-Service, in that that clients with an HVV card who logged in
the CES could access the data of other customers by changing the URL to
match their data profile.

</td>

</tr>

<tr>

<td style="text-align:right;">

247

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

04/04/2019

</td>

<td style="text-align:left;">

Company in the financial sector

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.baden-wuerttemberg.datenschutz.de/wp-content/uploads/2019/07/PM-Datenschutzverletzungen-bereiten-zunehmend-Sorge-30.07.2019.pdf>

</td>

<td style="text-align:left;">

The fine was issued because, in April 2019, the company hadn’t taken the
necessary measures to ensure the integrity and confidentiality of
information (as per Art. 5 para. 1 lit. f GDPR) when it disposed of
documents that contained personal information of two clients. We should
mention that the documents were simply disposed of in the general waste
recycling system where they were found by a neighbor.

</td>

</tr>

<tr>

<td style="text-align:right;">

248

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

200

</td>

<td style="text-align:left;">

Data Protection Authority of Nordrhein-Westfalen

</td>

<td style="text-align:left;">

08/05/2019

</td>

<td style="text-align:left;">

Private person (YouTube-Channel)

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<https://www.youtube.com/watch?v=wFBrgJIkDwI>

</td>

<td style="text-align:left;">

The private person has an insufficient legal basis for using a dashcam
to record public road traffic and publish it on YouTube as being part of
a compilation.

</td>

</tr>

<tr>

<td style="text-align:right;">

249

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/germany.svg>

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

100000

</td>

<td style="text-align:left;">

Data Protection Authority of Baden-Wuerttemberg

</td>

<td style="text-align:left;">

10/24/2019

</td>

<td style="text-align:left;">

Food company

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to implement sufficient measures to ensure information security

</td>

<td style="text-align:left;">

<https://www.baden-wuerttemberg.datenschutz.de/wp-content/uploads/2020/01/35.-T%C3%A4tigkeitsbericht-f%C3%BCr-den-Datenschutz-Web.pdf#page=44&zoom=100,0,0>

</td>

<td style="text-align:left;">

Upon creation of an applicant portal where interested parties could
apply their documents for a job, the food company failed to encrypt the
applicant portal. The transmission of the data had no encryption and the
data storage was completely unencrypted and offered no
password-protected security systems. Moreover, the data was linked to
Google, so anyone could find the applicants&\#8217; documents and
retrieve them after a simple Google search.

</td>

</tr>

<tr>

<td style="text-align:right;">

250

</td>

<td style="text-align:left;">

<https://www.privacyaffairs.com/wp-content/uploads/2019/10/greece.svg>

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Hellenic Data Protection Authority (HDPA)

</td>

<td style="text-align:left;">

12/19/2019

</td>

<td style="text-align:left;">

Aegean Marine Petroleum Network Inc. 

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Failure to comply with data processing principles

</td>

<td style="text-align:left;">

<http://www.dpa.gr/APDPXPortlets/htdocs/documentDisplay.jsp?docid=205,136,113,56,60,108,243,88>

</td>

<td style="text-align:left;">

The Aegean Marine Petroleum Network Inc. failed to inform data subjects
that they would have their data processed and stored on the servers.
Moreover, the company failed to impose the necessary technical measures
and secure the processing of such large amounts of data, while also
failing to impose a separation between the relevant software and the
data stored on the servers. As a result, companies outside the Aegean
Marine Petroleum Group had access to these servers and, implicitly, to
the personal data of data subjects, which they copied from the servers.

</td>

</tr>

</tbody>

</table>

</div>

How are the fines distributed?

``` r
gdpr_raw_histogram <- gdpr_raw %>%
  ggplot(aes(price + 1)) +
  geom_histogram(fill = "midnightblue", alpha = 0.7) +
  scale_x_log10(labels = scales::dollar_format(prefix = "€")) +
  labs(
    title = "EU General Data Protection Regulation 2016/679 (GDPR)",
    subtitle = "Scraped from https://www.privacyaffairs.com/gdpr-fines/",
    x = "GDPR fine (EUR)", y = "Number of GDPR violations",
    caption = "@Jim_Gruman | #TidyTuesday"
  ) +
  theme(plot.title.position = "plot")
gdpr_raw_histogram
```

![](GDPRfines_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Some of the violations were fined zero EUR. Let’s make a
one-article-per-row version of this dataset.

``` r
gdpr_tidy <- gdpr_raw %>%
  transmute(id,
    price,
    country = name,
    article_violated,
    articles = str_extract_all(article_violated, "Art.[:digit:]+|Art. [:digit:]+")
  ) %>%
  mutate(total_articles = map_int(articles, length)) %>%
  unnest(articles) %>%
  add_count(articles) %>%
  filter(n > 10) %>%
  select(-n)

gdpr_tidy %>%
  knitr::kable("html") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = F, fixed_thead = T
  ) %>%
  kableExtra::scroll_box(width = "800px", height = "200px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:800px; ">

<table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

id

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

price

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

country

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

article\_violated

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

articles

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

total\_articles

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 13 GDPR|Art. 5 (1) c) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 13 GDPR|Art. 5 (1) c) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 13 GDPR|Art. 5 (1) c) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

200000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 25 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

195407

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 15 GDPR|Art. 17 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

12

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

644780

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

14

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 13 GDPR|Art. 37 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

15

</td>

<td style="text-align:right;">

2600000

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

511000

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

18630

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 9 GDPR|Art. 35 GDPR|Art. 36 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

20

</td>

<td style="text-align:right;">

11000

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 5 (2) GDPR|Art. 6 (1) GDPR|Art. 13 (1) c) GDPR|Art.
14 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 5 (2) GDPR|Art. 6 (1) GDPR|Art. 13 (1) c) GDPR|Art.
14 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 5 (2) GDPR|Art. 6 (1) GDPR|Art. 13 (1) c) GDPR|Art.
14 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 5 (2) GDPR|Art. 6 (1) GDPR|Art. 13 (1) c) GDPR|Art.
14 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

22

</td>

<td style="text-align:right;">

180000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

23

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

24

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

25

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

26

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

27

</td>

<td style="text-align:right;">

130000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 25 (1) GDPR|Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

29

</td>

<td style="text-align:right;">

460000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

30

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 12 GDPR|Art. 13 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

30

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 12 GDPR|Art. 13 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

30

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 12 GDPR|Art. 13 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

31

</td>

<td style="text-align:right;">

250000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) a)|Art. 7 (3) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

32

</td>

<td style="text-align:right;">

200850

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:left;">

Art. 5 (1) e) GDPR|Art. 5 (2) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

32

</td>

<td style="text-align:right;">

200850

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:left;">

Art. 5 (1) e) GDPR|Art. 5 (2) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

33

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

33

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

34

</td>

<td style="text-align:right;">

400000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

35

</td>

<td style="text-align:right;">

92146

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) b) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

35

</td>

<td style="text-align:right;">

92146

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) b) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

35

</td>

<td style="text-align:right;">

92146

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) b) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

36

</td>

<td style="text-align:right;">

61500

</td>

<td style="text-align:left;">

Lithuania

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

36

</td>

<td style="text-align:right;">

61500

</td>

<td style="text-align:left;">

Lithuania

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:right;">

3105

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) b) GDPR|Art. 32 (1) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:right;">

3105

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) b) GDPR|Art. 32 (1) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:right;">

3105

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) b) GDPR|Art. 32 (1) GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

38

</td>

<td style="text-align:right;">

1400

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

39

</td>

<td style="text-align:right;">

194

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

40

</td>

<td style="text-align:right;">

203000

</td>

<td style="text-align:left;">

Norway

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

41

</td>

<td style="text-align:right;">

12950

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

43

</td>

<td style="text-align:right;">

9400

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

43

</td>

<td style="text-align:right;">

9400

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

44

</td>

<td style="text-align:right;">

510

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 9 (1) GDPR|Art. 9 (2) GDPR|Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

44

</td>

<td style="text-align:right;">

510

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 9 (1) GDPR|Art. 9 (2) GDPR|Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

46

</td>

<td style="text-align:right;">

1900

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

47

</td>

<td style="text-align:right;">

170000

</td>

<td style="text-align:left;">

Norway

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

47

</td>

<td style="text-align:right;">

170000

</td>

<td style="text-align:left;">

Norway

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

49

</td>

<td style="text-align:right;">

5100

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

49

</td>

<td style="text-align:right;">

5100

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

9704

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 5 (1) e) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

9704

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 5 (1) e) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 5 (1) c) GDPR|Art. 13 (3) GDPR|Art. 17 (1)
GDPR|Art. 6 (4) GDRP

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 5 (1) c) GDPR|Art. 13 (3) GDPR|Art. 17 (1)
GDPR|Art. 6 (4) GDRP

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 5 (1) c) GDPR|Art. 13 (3) GDPR|Art. 17 (1)
GDPR|Art. 6 (4) GDRP

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 5 (1) c) GDPR|Art. 13 (3) GDPR|Art. 17 (1)
GDPR|Art. 6 (4) GDRP

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

53

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

54

</td>

<td style="text-align:right;">

582

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

55

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

55

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

56

</td>

<td style="text-align:right;">

27100

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

56

</td>

<td style="text-align:right;">

27100

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

57

</td>

<td style="text-align:right;">

776

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

58

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

59

</td>

<td style="text-align:right;">

1560

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

59

</td>

<td style="text-align:right;">

1560

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

60

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Malta

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

60

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Malta

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

61

</td>

<td style="text-align:right;">

1560

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 5 (1) d) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

62

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

62

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

63

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

64

</td>

<td style="text-align:right;">

1165

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

65

</td>

<td style="text-align:right;">

1165

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

50000000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 13 GDPR|Art. 14 GDPR|Art. 6 GDPR|Art. 4 GDPR|Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

50000000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 13 GDPR|Art. 14 GDPR|Art. 6 GDPR|Art. 4 GDPR|Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

50000000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 13 GDPR|Art. 14 GDPR|Art. 6 GDPR|Art. 4 GDPR|Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

67

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

67

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

68

</td>

<td style="text-align:right;">

388

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

69

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

69

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:right;">

2200

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR|Art. 6 (1) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:right;">

2200

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR|Art. 6 (1) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:right;">

2200

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR|Art. 6 (1) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:right;">

2200

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 5 (1) c) GDPR|Art. 6 (1) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

71

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 12 (4) GDPR|Art. 15 GDPR|Art. 18 (1) c) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

71

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 12 (4) GDPR|Art. 15 GDPR|Art. 18 (1) c) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

73

</td>

<td style="text-align:right;">

4800

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

74

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

75

</td>

<td style="text-align:right;">

388

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

76

</td>

<td style="text-align:right;">

300

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

76

</td>

<td style="text-align:right;">

300

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

77

</td>

<td style="text-align:right;">

400000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

77

</td>

<td style="text-align:right;">

400000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

78

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

78

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 5 (1) b) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

79

</td>

<td style="text-align:right;">

18000000

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

79

</td>

<td style="text-align:right;">

18000000

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

80

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

81

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

82

</td>

<td style="text-align:right;">

14500000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 25 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

83

</td>

<td style="text-align:right;">

36000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

83

</td>

<td style="text-align:right;">

36000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

90

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) d) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

91

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

92

</td>

<td style="text-align:right;">

27000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) d) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

93

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

94

</td>

<td style="text-align:right;">

12000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

94

</td>

<td style="text-align:right;">

12000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

95

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

96

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

96

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

97

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

97

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

98

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

98

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR|Art. 6 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

101

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

103

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

104

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 25 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

106

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

107

</td>

<td style="text-align:right;">

160000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:left;">

Art. 5(1) e) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

108

</td>

<td style="text-align:right;">

900000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

109

</td>

<td style="text-align:right;">

47000

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

110

</td>

<td style="text-align:right;">

12000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

111

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

112

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

113

</td>

<td style="text-align:right;">

900

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

114

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

115

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

116

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

117

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

119

</td>

<td style="text-align:right;">

11000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

120

</td>

<td style="text-align:right;">

500000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 13 GDPR|Art. 14 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

120

</td>

<td style="text-align:right;">

500000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 13 GDPR|Art. 14 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

120

</td>

<td style="text-align:right;">

500000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 13 GDPR|Art. 14 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

5

</td>

</tr>

<tr>

<td style="text-align:right;">

121

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Latvia

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

122

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

123

</td>

<td style="text-align:right;">

3140

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

124

</td>

<td style="text-align:right;">

588

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 7 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

125

</td>

<td style="text-align:right;">

980

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

126

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

127

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

128

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

129

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

130

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

132

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

133

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

134

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

135

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

136

</td>

<td style="text-align:right;">

105000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

138

</td>

<td style="text-align:right;">

9550000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

139

</td>

<td style="text-align:right;">

28160

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

140

</td>

<td style="text-align:right;">

15100

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

141

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

142

</td>

<td style="text-align:right;">

21000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

143

</td>

<td style="text-align:right;">

36000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

144

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

145

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

145

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

146

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

147

</td>

<td style="text-align:right;">

3600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

148

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

149

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

150

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) a) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

151

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 12 GDPR|Art. 15 GDPR|Art. 17 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

152

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

153

</td>

<td style="text-align:right;">

90

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

154

</td>

<td style="text-align:right;">

7400

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:left;">

Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

155

</td>

<td style="text-align:right;">

1600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

155

</td>

<td style="text-align:right;">

1600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

156

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

157

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

158

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

159

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

160

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

161

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

162

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

165

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

167

</td>

<td style="text-align:right;">

320000

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

168

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

170

</td>

<td style="text-align:right;">

35000

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

171

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 12 GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

171

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 12 GDPR|Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

173

</td>

<td style="text-align:right;">

11760

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

174

</td>

<td style="text-align:right;">

5113

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

175

</td>

<td style="text-align:right;">

5112

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

176

</td>

<td style="text-align:right;">

1121

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 12 (4) GDPR|Art. 15 GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

177

</td>

<td style="text-align:right;">

1022

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 6 (1) GDPR|Art. 25 (1) GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

178

</td>

<td style="text-align:right;">

511

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:left;">

Art. 12 (3) GDPR|Art. 15 (1) GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

180

</td>

<td style="text-align:right;">

800

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

181

</td>

<td style="text-align:right;">

118

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

182

</td>

<td style="text-align:right;">

294000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

183

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

184

</td>

<td style="text-align:right;">

1000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

185

</td>

<td style="text-align:right;">

70000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

186

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

187

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

188

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 (1) a), (2) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

189

</td>

<td style="text-align:right;">

8500000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 17 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

189

</td>

<td style="text-align:right;">

8500000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 17 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

190

</td>

<td style="text-align:right;">

3000000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

190

</td>

<td style="text-align:right;">

3000000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

191

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 7 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

191

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 7 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

192

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 6 GDPR|Art. 7 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

192

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 6 GDPR|Art. 7 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

193

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 6 GDPR|Art. 7 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

193

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR|Art. 6 GDPR|Art. 7 GDPR|Art. 9 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

194

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 25 GDPR|Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

194

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 25 GDPR|Art. 32 GDPR|Art. 33 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:right;">

196

</td>

<td style="text-align:right;">

44000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

197

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

198

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

199

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

201

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

201

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

202

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

202

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

203

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

203

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

204

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

205

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

205

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

206

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

206

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

207

</td>

<td style="text-align:right;">

6670

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

207

</td>

<td style="text-align:right;">

6670

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

208

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

208

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

209

</td>

<td style="text-align:right;">

800

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

209

</td>

<td style="text-align:right;">

800

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

210

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

210

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

211

</td>

<td style="text-align:right;">

3600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

211

</td>

<td style="text-align:right;">

3600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

212

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

212

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

214

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

215

</td>

<td style="text-align:right;">

120000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

216

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

216

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

217

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

218

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

219

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

220

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

221

</td>

<td style="text-align:right;">

42000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

222

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

223

</td>

<td style="text-align:right;">

24000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

223

</td>

<td style="text-align:right;">

24000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

224

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

225

</td>

<td style="text-align:right;">

42000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

226

</td>

<td style="text-align:right;">

1800

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 13 GDPR

</td>

<td style="text-align:left;">

Art. 13

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

227

</td>

<td style="text-align:right;">

525000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

227

</td>

<td style="text-align:right;">

525000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

228

</td>

<td style="text-align:right;">

4600

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

229

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

230

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

230

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

231

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

231

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

232

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

233

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

233

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

234

</td>

<td style="text-align:right;">

20600

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

234

</td>

<td style="text-align:right;">

20600

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

235

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

235

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

236

</td>

<td style="text-align:right;">

7000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

236

</td>

<td style="text-align:right;">

7000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

237

</td>

<td style="text-align:right;">

7000000

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 17 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

237

</td>

<td style="text-align:right;">

7000000

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 17 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

238

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

238

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 5 (1) f) GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

239

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 6 GDPR|Art. 21 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

240

</td>

<td style="text-align:right;">

4150

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

241

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:left;">

Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

243

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Croatia

</td>

<td style="text-align:left;">

Art. 15 (1), (3) GDPR

</td>

<td style="text-align:left;">

Art. 15

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

244

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:left;">

Art. 5 (1) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

245

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 (1) c) GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

247

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

247

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

248

</td>

<td style="text-align:right;">

200

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

248

</td>

<td style="text-align:right;">

200

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

249

</td>

<td style="text-align:right;">

100000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

249

</td>

<td style="text-align:right;">

100000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:right;">

250

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 5

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

250

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 6

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:right;">

250

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:left;">

Art. 5 GDPR|Art. 6 GDPR|Art. 32 GDPR

</td>

<td style="text-align:left;">

Art. 32

</td>

<td style="text-align:right;">

3

</td>

</tr>

</tbody>

</table>

</div>

How are the fines distributed by article?

``` r
library(ggbeeswarm)

gdpr_fines_levied <- gdpr_tidy %>%
  mutate(
    articles = str_replace_all(articles, "Art. ", "Article "),
    articles = fct_reorder(articles, price)
  ) %>%
  ggplot(aes(articles, price + 1, color = articles, fill = articles)) +
  geom_boxplot(alpha = 0.2, outlier.colour = NA) +
  geom_quasirandom() +
  scale_y_log10(labels = scales::dollar_format(prefix = "€")) +
  labs(
    x = NULL, y = "GDPR fine (EUR)",
    title = "GDPR Fines Levied, by Article",
    subtitle = "For 250 violations in 25 countries",
    caption = "@Jim_Gruman | #TidyTuesday"
  ) +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  )
gdpr_fines_levied
```

![](GDPRfines_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Now let’s create a dataset for modeling.

``` r
gdpr_violations <- gdpr_tidy %>%
  mutate(value = 1) %>%
  select(-article_violated) %>%
  pivot_wider(
    names_from = articles, values_from = value,
    values_fn = list(value = max), values_fill = list(value = 0)
  ) %>%
  janitor::clean_names()

gdpr_violations %>%
  knitr::kable("html") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = F, fixed_thead = T
  ) %>%
  kableExtra::scroll_box(width = "800px", height = "200px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:800px; ">

<table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

id

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

price

</th>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

country

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

total\_articles

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_13

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_5

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_6

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_32

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_15

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

200000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

195407

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

12

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

644780

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

14

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

15

</td>

<td style="text-align:right;">

2600000

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

511000

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

18630

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

20

</td>

<td style="text-align:right;">

11000

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

22

</td>

<td style="text-align:right;">

180000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

23

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

24

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

25

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

26

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

27

</td>

<td style="text-align:right;">

130000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

29

</td>

<td style="text-align:right;">

460000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

30

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

31

</td>

<td style="text-align:right;">

250000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

32

</td>

<td style="text-align:right;">

200850

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

33

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

34

</td>

<td style="text-align:right;">

400000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

35

</td>

<td style="text-align:right;">

92146

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

36

</td>

<td style="text-align:right;">

61500

</td>

<td style="text-align:left;">

Lithuania

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

37

</td>

<td style="text-align:right;">

3105

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

38

</td>

<td style="text-align:right;">

1400

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

39

</td>

<td style="text-align:right;">

194

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

40

</td>

<td style="text-align:right;">

203000

</td>

<td style="text-align:left;">

Norway

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

41

</td>

<td style="text-align:right;">

12950

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

42

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

43

</td>

<td style="text-align:right;">

9400

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

44

</td>

<td style="text-align:right;">

510

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

46

</td>

<td style="text-align:right;">

1900

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

47

</td>

<td style="text-align:right;">

170000

</td>

<td style="text-align:left;">

Norway

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

49

</td>

<td style="text-align:right;">

5100

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

50

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

51

</td>

<td style="text-align:right;">

9704

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

52

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

53

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

54

</td>

<td style="text-align:right;">

582

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

55

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

56

</td>

<td style="text-align:right;">

27100

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

57

</td>

<td style="text-align:right;">

776

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

58

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

59

</td>

<td style="text-align:right;">

1560

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

60

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Malta

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

61

</td>

<td style="text-align:right;">

1560

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

62

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

63

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

64

</td>

<td style="text-align:right;">

1165

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

65

</td>

<td style="text-align:right;">

1165

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

50000000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

67

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

68

</td>

<td style="text-align:right;">

388

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

69

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

70

</td>

<td style="text-align:right;">

2200

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

71

</td>

<td style="text-align:right;">

3200

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

73

</td>

<td style="text-align:right;">

4800

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

74

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

75

</td>

<td style="text-align:right;">

388

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

76

</td>

<td style="text-align:right;">

300

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

77

</td>

<td style="text-align:right;">

400000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

78

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

79

</td>

<td style="text-align:right;">

18000000

</td>

<td style="text-align:left;">

Austria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

80

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

81

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

82

</td>

<td style="text-align:right;">

14500000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

83

</td>

<td style="text-align:right;">

36000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

90

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

91

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

92

</td>

<td style="text-align:right;">

27000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

93

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

94

</td>

<td style="text-align:right;">

12000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

95

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

96

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

97

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

98

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

100

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

101

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

103

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

104

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

106

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

107

</td>

<td style="text-align:right;">

160000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

108

</td>

<td style="text-align:right;">

900000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

109

</td>

<td style="text-align:right;">

47000

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

110

</td>

<td style="text-align:right;">

12000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

111

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

112

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

113

</td>

<td style="text-align:right;">

900

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

114

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

115

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

116

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

117

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

119

</td>

<td style="text-align:right;">

11000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

120

</td>

<td style="text-align:right;">

500000

</td>

<td style="text-align:left;">

France

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

121

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Latvia

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

122

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

123

</td>

<td style="text-align:right;">

3140

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

124

</td>

<td style="text-align:right;">

588

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

125

</td>

<td style="text-align:right;">

980

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

126

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Portugal

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

127

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

128

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

129

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Slovakia

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

130

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

132

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

133

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

134

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

135

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

136

</td>

<td style="text-align:right;">

105000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

138

</td>

<td style="text-align:right;">

9550000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

139

</td>

<td style="text-align:right;">

28160

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

140

</td>

<td style="text-align:right;">

15100

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

141

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

142

</td>

<td style="text-align:right;">

21000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

143

</td>

<td style="text-align:right;">

36000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

144

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

145

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

146

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

147

</td>

<td style="text-align:right;">

3600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

148

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

149

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

150

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

151

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

152

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

153

</td>

<td style="text-align:right;">

90

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

154

</td>

<td style="text-align:right;">

7400

</td>

<td style="text-align:left;">

Hungary

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

155

</td>

<td style="text-align:right;">

1600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

156

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

157

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

158

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

159

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

160

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

161

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

162

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

165

</td>

<td style="text-align:right;">

500

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

167

</td>

<td style="text-align:right;">

320000

</td>

<td style="text-align:left;">

United Kingdom

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

168

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

170

</td>

<td style="text-align:right;">

35000

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

171

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Belgium

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

173

</td>

<td style="text-align:right;">

11760

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

174

</td>

<td style="text-align:right;">

5113

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

175

</td>

<td style="text-align:right;">

5112

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

176

</td>

<td style="text-align:right;">

1121

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

177

</td>

<td style="text-align:right;">

1022

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

178

</td>

<td style="text-align:right;">

511

</td>

<td style="text-align:left;">

Bulgaria

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

180

</td>

<td style="text-align:right;">

800

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

181

</td>

<td style="text-align:right;">

118

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

182

</td>

<td style="text-align:right;">

294000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

183

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

184

</td>

<td style="text-align:right;">

1000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

185

</td>

<td style="text-align:right;">

70000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

186

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

187

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Cyprus

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

188

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

189

</td>

<td style="text-align:right;">

8500000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

190

</td>

<td style="text-align:right;">

3000000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

191

</td>

<td style="text-align:right;">

6000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

192

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

193

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

194

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

196

</td>

<td style="text-align:right;">

44000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

197

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

198

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

199

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

201

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

202

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

203

</td>

<td style="text-align:right;">

60000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

204

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

205

</td>

<td style="text-align:right;">

20000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

206

</td>

<td style="text-align:right;">

75000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

207

</td>

<td style="text-align:right;">

6670

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

208

</td>

<td style="text-align:right;">

5000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

209

</td>

<td style="text-align:right;">

800

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

210

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

211

</td>

<td style="text-align:right;">

3600

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

212

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

214

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

215

</td>

<td style="text-align:right;">

120000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

216

</td>

<td style="text-align:right;">

48000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

217

</td>

<td style="text-align:right;">

1500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

218

</td>

<td style="text-align:right;">

2500

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

219

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

220

</td>

<td style="text-align:right;">

50000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

221

</td>

<td style="text-align:right;">

42000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

222

</td>

<td style="text-align:right;">

30000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

223

</td>

<td style="text-align:right;">

24000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

224

</td>

<td style="text-align:right;">

40000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

225

</td>

<td style="text-align:right;">

42000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

226

</td>

<td style="text-align:right;">

1800

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

227

</td>

<td style="text-align:right;">

525000

</td>

<td style="text-align:left;">

Netherlands

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

228

</td>

<td style="text-align:right;">

4600

</td>

<td style="text-align:left;">

Poland

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

229

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

230

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

231

</td>

<td style="text-align:right;">

4000

</td>

<td style="text-align:left;">

Italy

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

232

</td>

<td style="text-align:right;">

15000

</td>

<td style="text-align:left;">

Spain

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

233

</td>

<td style="text-align:right;">

9000

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

234

</td>

<td style="text-align:right;">

20600

</td>

<td style="text-align:left;">

Iceland

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

235

</td>

<td style="text-align:right;">

14000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

236

</td>

<td style="text-align:right;">

7000

</td>

<td style="text-align:left;">

Denmark

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

237

</td>

<td style="text-align:right;">

7000000

</td>

<td style="text-align:left;">

Sweden

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

238

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

239

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

240

</td>

<td style="text-align:right;">

4150

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

241

</td>

<td style="text-align:right;">

3000

</td>

<td style="text-align:left;">

Romania

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

243

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:left;">

Croatia

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

244

</td>

<td style="text-align:right;">

10000

</td>

<td style="text-align:left;">

Czech Republic

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

245

</td>

<td style="text-align:right;">

2000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

247

</td>

<td style="text-align:right;">

80000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

248

</td>

<td style="text-align:right;">

200

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

249

</td>

<td style="text-align:right;">

100000

</td>

<td style="text-align:left;">

Germany

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

250

</td>

<td style="text-align:right;">

150000

</td>

<td style="text-align:left;">

Greece

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

</tbody>

</table>

</div>

We are ready to go\!

## Build a model

Let’s preprocess our data to get it ready for modeling.

``` r
gdpr_rec <- recipe(price ~ ., data = gdpr_violations) %>%
  update_role(id, new_role = "id") %>%
  step_log(price, base = 10, offset = 1, skip = TRUE) %>%
  step_other(country, other = "Other") %>%
  step_dummy(all_nominal()) %>%
  step_zv(all_predictors())

gdpr_prep <- prep(gdpr_rec)

gdpr_prep
```

    ## Data Recipe
    ## 
    ## Inputs:
    ## 
    ##       role #variables
    ##         id          1
    ##    outcome          1
    ##  predictor          7
    ## 
    ## Training data contained 219 data points and no missing data.
    ## 
    ## Operations:
    ## 
    ## Log transformation on price [trained]
    ## Collapsing factor levels for country [trained]
    ## Dummy variables from country [trained]
    ## Zero variance filter removed no terms [trained]

Let’s walk through the steps in this recipe.

  - First, we must tell the `recipe()` what our model is going to be
    (using a formula here) and what data we are using.
  - Next, we update the role for `id`, since this variable is not a
    predictor or outcome but I would like to keep it in the data for
    convenience.
  - Next, we take the log of the outcome (`price`, the amount of the
    fine).
  - There are a lot of countries in this dataset, so let’s collapse some
    of the less frequently occurring countries into another `"Other"`
    category.
  - Finally, we can create indicator variables and remove varibles with
    zero variance.

Before using `prep()` these steps have been defined but not actually run
or implemented. The `prep()` function is where everything gets
evaluated.

Now it’s time to specify our model. I am using a
[`workflow()`](https://tidymodels.github.io/workflows/) in this example
for convenience; these are objects that can help you manage modeling
pipelines more easily, with pieces that fit together like Lego blocks.
This `workflow()` contains both the recipe and the model (a
straightforward Ordinary Least Squares linear regression).

``` r
gdpr_wf <- workflow() %>%
  add_recipe(gdpr_rec) %>%
  add_model(linear_reg() %>%
    set_engine("lm"))

gdpr_wf
```

    ## == Workflow ============================================================================
    ## Preprocessor: Recipe
    ## Model: linear_reg()
    ## 
    ## -- Preprocessor ------------------------------------------------------------------------
    ## 4 Recipe Steps
    ## 
    ## * step_log()
    ## * step_other()
    ## * step_dummy()
    ## * step_zv()
    ## 
    ## -- Model -------------------------------------------------------------------------------
    ## Linear Regression Model Specification (regression)
    ## 
    ## Computational engine: lm

You can `fit()` a workflow, much like you can fit a model, and then you
can pull out the fit object and `tidy()` it\!

``` r
gdpr_fit <- gdpr_wf %>%
  fit(data = gdpr_violations)

# gdpr_fit %>%
#  workflows::pull_workflow_fit() %>%
#  tidy() %>%
#  arrange(estimate) %>%
#  kable()
```

GDPR violations of more than one article have higher fines.

## Explore results

Lots of those coefficients have big p-values (for example, all the
countries) but I think the best way to understand these results will be
to visualize some predictions. You can predict on new data in tidymodels
with either a model or a `workflow()`.

Let’s create some example new data that we are interested in.

``` r
new_gdpr <- crossing(
  country = "Other",
  art_5 = 0:1,
  art_6 = 0:1,
  art_13 = 0:1,
  art_15 = 0:1,
  art_32 = 0:1
) %>%
  mutate(
    id = row_number(),
    total_articles = art_5 + art_6 + art_13 + art_15 + art_32
  )

new_gdpr %>%
  knitr::kable("html") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = F, fixed_thead = T
  ) %>%
  kableExtra::scroll_box(width = "800px", height = "200px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:800px; ">

<table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

country

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_5

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_6

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_13

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_15

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_32

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

id

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

total\_articles

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

12

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

15

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

17

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

20

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

22

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

23

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

24

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

25

</td>

<td style="text-align:right;">

2

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

26

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

27

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

28

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

29

</td>

<td style="text-align:right;">

3

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

30

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

31

</td>

<td style="text-align:right;">

4

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

32

</td>

<td style="text-align:right;">

5

</td>

</tr>

</tbody>

</table>

</div>

Let’s find both the mean predictions and the confidence intervals.

``` r
mean_pred <- predict(gdpr_fit,
  new_data = new_gdpr
)

conf_int_pred <- predict(gdpr_fit,
  new_data = new_gdpr,
  type = "conf_int"
)

gdpr_res <- new_gdpr %>%
  bind_cols(mean_pred) %>%
  bind_cols(conf_int_pred)

gdpr_res %>%
  knitr::kable("html") %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = F, fixed_thead = T
  ) %>%
  kableExtra::scroll_box(width = "800px", height = "200px")
```

<div style="border: 1px solid #ddd; padding: 0px; overflow-y: scroll; height:200px; overflow-x: scroll; width:800px; ">

<table class="table table-striped table-condensed" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

country

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_5

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_6

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_13

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_15

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

art\_32

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

id

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

total\_articles

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

.pred

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

.pred\_lower

</th>

<th style="text-align:right;position: sticky; top:0; background-color: #FFFFFF;position: sticky; top:0; background-color: #FFFFFF;">

.pred\_upper

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

4.000446

</td>

<td style="text-align:right;">

3.410428

</td>

<td style="text-align:right;">

4.590464

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

4.326841

</td>

<td style="text-align:right;">

3.922444

</td>

<td style="text-align:right;">

4.731237

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

2.912359

</td>

<td style="text-align:right;">

2.245405

</td>

<td style="text-align:right;">

3.579314

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3.238753

</td>

<td style="text-align:right;">

2.407347

</td>

<td style="text-align:right;">

4.070160

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

3.717506

</td>

<td style="text-align:right;">

2.992813

</td>

<td style="text-align:right;">

4.442199

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

4.043900

</td>

<td style="text-align:right;">

3.336772

</td>

<td style="text-align:right;">

4.751029

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

7

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2.629419

</td>

<td style="text-align:right;">

1.712386

</td>

<td style="text-align:right;">

3.546452

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

2.955813

</td>

<td style="text-align:right;">

1.839215

</td>

<td style="text-align:right;">

4.072412

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

9

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

3.920137

</td>

<td style="text-align:right;">

3.484573

</td>

<td style="text-align:right;">

4.355701

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

10

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

4.246531

</td>

<td style="text-align:right;">

3.686407

</td>

<td style="text-align:right;">

4.806654

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

11

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2.832050

</td>

<td style="text-align:right;">

2.022912

</td>

<td style="text-align:right;">

3.641187

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

12

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3.158444

</td>

<td style="text-align:right;">

2.058572

</td>

<td style="text-align:right;">

4.258316

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

13

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3.637196

</td>

<td style="text-align:right;">

2.944166

</td>

<td style="text-align:right;">

4.330227

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

14

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3.963591

</td>

<td style="text-align:right;">

3.089663

</td>

<td style="text-align:right;">

4.837518

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

15

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

2.549109

</td>

<td style="text-align:right;">

1.470063

</td>

<td style="text-align:right;">

3.628156

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

2.875504

</td>

<td style="text-align:right;">

1.504814

</td>

<td style="text-align:right;">

4.246193

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

17

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

4.061318

</td>

<td style="text-align:right;">

3.582229

</td>

<td style="text-align:right;">

4.540407

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

18

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

4.387712

</td>

<td style="text-align:right;">

3.934447

</td>

<td style="text-align:right;">

4.840978

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

2.973231

</td>

<td style="text-align:right;">

2.201963

</td>

<td style="text-align:right;">

3.744499

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

20

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3.299625

</td>

<td style="text-align:right;">

2.298740

</td>

<td style="text-align:right;">

4.300510

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3.778378

</td>

<td style="text-align:right;">

3.066970

</td>

<td style="text-align:right;">

4.489786

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

22

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

4.104772

</td>

<td style="text-align:right;">

3.303861

</td>

<td style="text-align:right;">

4.905684

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

23

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

2.690291

</td>

<td style="text-align:right;">

1.646025

</td>

<td style="text-align:right;">

3.734557

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

24

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

3.016685

</td>

<td style="text-align:right;">

1.729501

</td>

<td style="text-align:right;">

4.303869

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

25

</td>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

3.981008

</td>

<td style="text-align:right;">

3.570238

</td>

<td style="text-align:right;">

4.391779

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

26

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

4.307403

</td>

<td style="text-align:right;">

3.634154

</td>

<td style="text-align:right;">

4.980651

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

27

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

2.892921

</td>

<td style="text-align:right;">

1.943005

</td>

<td style="text-align:right;">

3.842838

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

28

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

3.219315

</td>

<td style="text-align:right;">

1.947363

</td>

<td style="text-align:right;">

4.491268

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

29

</td>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

3.698068

</td>

<td style="text-align:right;">

2.950522

</td>

<td style="text-align:right;">

4.445614

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

30

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

4.024462

</td>

<td style="text-align:right;">

3.023068

</td>

<td style="text-align:right;">

5.025856

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

31

</td>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

2.609981

</td>

<td style="text-align:right;">

1.380552

</td>

<td style="text-align:right;">

3.839410

</td>

</tr>

<tr>

<td style="text-align:left;">

Other

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

32

</td>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

2.936375

</td>

<td style="text-align:right;">

1.391560

</td>

<td style="text-align:right;">

4.481190

</td>

</tr>

</tbody>

</table>

</div>

There are lots of things we can do wtih these results\! For example,
what are the predicted GDPR fines for violations of each article type
(violating only one article)?

``` r
pred_fine <- gdpr_res %>%
  filter(total_articles == 1) %>%
  pivot_longer(art_5:art_32) %>%
  filter(value > 0) %>%
  mutate(
    name = str_replace_all(name, "art_", "Article "),
    name = fct_reorder(name, .pred)
  ) %>%
  ggplot(aes(name, 10^.pred, color = name)) +
  geom_point(size = 3.5) +
  geom_errorbar(aes(
    ymin = 10^.pred_lower,
    ymax = 10^.pred_upper
  ),
  width = 0.2, alpha = 0.7
  ) +
  labs(
    x = NULL, y = "Increase in fine (EUR)",
    title = "Predicted Fine for each Type of GDPR Article Violation",
    subtitle = "Modeling based on 250 violations in 25 countries",
    caption = "@Jim_Gruman | #TidyTuesday"
  ) +
  scale_y_log10(labels = scales::dollar_format(prefix = "€", accuracy = 1)) +
  theme(
    legend.position = "none",
    plot.title.position = "plot"
  )
pred_fine
```

![](GDPRfines_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

We can see here that violations such as data breaches have higher fines
on average than violations about rights of access.

``` r
# Get countries in dataset as a vector
gdpr_countries <- gdpr_raw %>%
  distinct(name) %>%
  pull()

# Get sf objects, filter by countries in dataset
countries_sf <- rnaturalearth::ne_countries(country = c(gdpr_countries, "Czechia"), scale = "large", returnclass = "sf") %>%
  select(name, geometry) %>%
  mutate(name = replace(name, name == "Czechia", "Czech Republic"))

# Group fines by country, merge with sf
countries_map <- gdpr_raw %>%
  mutate(name = stringr::str_to_title(name)) %>%
  group_by(name) %>%
  mutate(
    price_sum = sum(price),
    price_label = case_when(
      round(price_sum / 1e6) > 0 ~ paste0(round(price_sum / 1e6), "M"),
      round(price_sum / 1e5) > 0 ~ paste0(round(price_sum / 1e6, 1), "M"),
      round(price_sum / 1e3) > 0 ~ paste0(round(price_sum / 1e3), "K"),
      price_sum > 0 ~ paste0(round(price_sum / 1e3, 1), " K"),
      TRUE ~ "0"
    )
  ) %>%
  left_join(countries_sf) %>%
  select(name, price_sum, price_label, geometry)

# Copied from https://developers.google.com/public-data/docs/canonical/countries_csv
centroids <- read_html("https://developers.google.com/public-data/docs/canonical/countries_csv") %>%
  html_node("table") %>%
  html_table()

# Dataset for red "arrows" (to draw with geom_polygon)
price_arrows <- countries_map %>%
  select(name, price_sum, price_label) %>%
  left_join(centroids) %>%
  mutate(
    arrow_x = list(c(longitude - 0.5, longitude, longitude + 0.5, longitude)),
    arrow_y = list(c(latitude - 0.03, latitude, latitude - 0.03, latitude + price_sum / 1.5e6))
  ) %>%
  unnest(c(arrow_x, arrow_y))

gdpr_map <- ggplot() +
  # map
  geom_sf(data = countries_map, aes(geometry = geometry), fill = "#EBE9E1", colour = "grey70", size = 0.25) +
  # country name
  geom_text(data = price_arrows, aes(x = longitude - 0.2, y = latitude - 0.4, label = name), check_overlap = TRUE, hjust = 0, vjust = 1, size = 3.5) +
  # red price, over 10M
  geom_text(data = subset(price_arrows, price_sum > 10e6), aes(x = longitude - 0.2, y = latitude - 2, label = price_label), check_overlap = TRUE, hjust = 0, vjust = 1, size = 3.5, colour = "#BA4E35") +
  # black price, under 10M
  geom_text(data = subset(price_arrows, price_sum < 10e6), aes(x = longitude - 0.2, y = latitude - 2, label = price_label), check_overlap = TRUE, hjust = 0, vjust = 1, size = 3.5, colour = "black") +
  # red arrows
  geom_polygon(data = price_arrows, aes(x = arrow_x, y = arrow_y, group = name), fill = "#BA4E35", colour = NA, alpha = 0.8) +
  # title and caption
  annotate("richtext",
    x = -26, y = 80, hjust = 0, vjust = 1,
    label = "**Total of GDPR fines by country**<br><span style = 'font-size:12pt'>Rounded to nearest million or thousand euro</span><br><span style = 'font-size:8pt'>Source: Privacy Affairs | Graphic: @Jim_Gruman</span>",
    family = "IBM Plex Serif", size = 8, lineheight = 1.1, fill = NA, label.color = NA
  ) +
  theme_void() +
  theme(
    #    plot.margin = margin(20, 20, 20, 20),
    plot.title.position = "plot"
  ) +
  coord_sf(xlim = c(-27.5, 37.5), ylim = c(32.5, 82.5), expand = FALSE)

gdpr_map +
  ggsave("GDPRmap.png", dpi = 320, width = 14, height = 11)
```

![](GDPRfines_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
library(patchwork)
png("GDPR.png", width = 8, height = 12, units = "in", res = 120)

(pred_fine + gdpr_raw_histogram / gdpr_fines_levied) + plot_layout(heights = c(1, 3))

dev.off()
```

    ## png 
    ##   2
