# crude analysis of the correlation between average country performance and the average difference between private and public schools
# everything is really dirty but there seems to be no correlation between the two, quite surprisingly.

library(tidyverse)
library(downloader)
library(car)
library(Hmisc)
library(artyfarty)
library(stringr)
library(devtools)

# 
# ###### 2013 ###########
# Please check this link before running. This url depends heavily on folders and datasets which are
# outline there.

llece <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/TERCE/Experimental%20TERCE.r"
downloader::source_url(llece, sha = sha_url(llece))

terce <- terce("/Users/cimentadaj/Downloads/terce")

big <- function (data, var1, var2) {
  attach(data)
  out <- ifelse(is.na(var1) & is.na(var2), NA,
                ifelse(is.na(var1) & !is.na(var2), var2,
                       ifelse(!is.na(var1) & is.na(var2), var1,
                              ifelse(var1 > var2, var1, var2))))
  detach(data)
  out
}

terce <- rename(terce,  sesp = dqfit09_01_family, sesm = dqfit09_02_family)

terce$ses <- big(terce, sesp, sesm)
terce$ses2 <- car::recode(terce$ses, "1:2 = 1; 3:4 = 2; 5:6 = 3; 7 = NA")

terce$genero <- terce$genero - 1

terce$teacher_edu <- car::recode(terce$dqpit01_mteacher, "1:2 = 1; 3:4 = 2; 5:6 = 3; 7 = NA")
terce$country <- car::recode(terce$country, "
                             'ARG' = 'Argentina';
                             'BRA' = 'Brazil';
                             'CHL' = 'Chile';
                             'COL' = 'Colombia';
                             'CRI' = 'Costa Rica';
                             'ECU' = 'Ecuador';
                             'GTM' = 'Guatemala';
                             'HON' = 'Honduras';
                             'MEX' = 'Mexico';
                             'NIC' = 'Nicaragua';
                             'NLE' = 'Nuevo Leon (Mexico)';
                             'PAN' = 'Panama';
                             'PAR' = 'Paraguay';
                             'PER' = 'Peru';
                             'REP' = 'Dominican Republic';
                             'URU' = 'Uruguay'")

terce <- terce %>%
  select(ses2, country, puntaje_estandar_math, puntaje_estandar_language, wgm_math,
         wgl_language) %>%
  filter(!is.na(ses2) & !is.na(country)) %>%
  group_by(country, ses2) %>%
  do(data.frame(score = quantile(.$puntaje_estandar_math, probs = seq(0, 1, 0.01), na.rm = T))) %>%
  ungroup(country, ses2) %>%
  mutate(rank = rep(0:100, length(unique(country)) * length(unique(ses2))),
         year = "2013",
         survey = "LLECE")

# ###############
# 
# ##### 2007 #######
# # load serce function

serce_dir <- "https://raw.githubusercontent.com/cimentadaj/LLECE/master/SERCE/Exploratory_SERCE.R"
downloader::source_url(serce_dir, sha = sha_url(serce_dir))

serce <- serce("/Users/cimentadaj/Downloads/serce/SERCE/")

changer <- function(df, father, var_name) {
  # GEt the logitcal data frame in which the education is located
  logical_data <- df[grep(father, grep("qf_item_2_*", names(df), value = T), value = T)] == 1
  # Pick the highest education towards the end of the data frame
  df[var_name] <- max.col(logical_data, ties.method = "last")

  # Many respondents had empty rows yet the max.col assigned
  # them an education level. Let's assign an NA to those which had a row
  # full of NOT 1's

  row_logical <- apply(logical_data, 1, sum)
  df[, var_name] <- ifelse(row_logical != 0, df[, var_name], NA)

  df[, var_name]
}

serce$sesp <- changer(serce, "a_family", "sesp")
serce$sesm <- changer(serce, "b_family", "sesm")

big <- function (data, var1, var2) {
  attach(data)
  out <- ifelse(is.na(var1) & is.na(var2), NA,
                ifelse(is.na(var1) & !is.na(var2), var2,
                       ifelse(!is.na(var1) & is.na(var2), var1,
                              ifelse(var1 > var2, var1, var2))))
  detach(data)
  out
}

serce$ses <- big(serce, sesp, sesm)
serce$ses2 <- car::recode(as.numeric(serce$ses), "1:3 = 1; 4:5 = 2; 6:7 = 3")
serce$country <- car::recode(serce$country, "
                             '01' = 'Argentina';
                             '03' = 'Brazil';
                             '04' = 'Chile';
                             '05' = 'Colombia';
                             '06' = 'Costa Rica';
                             '07' = 'Cuba';
                             '08' = 'Ecuador';
                             '09' = 'El Salvador';
                             '11' = 'Guatemala';
                             '12' = 'Mexico';
                             '14' = 'Nicaragua';
                             '16' = 'Panama';
                             '17' = 'Paraguay';
                             '18' = 'Peru';
                             '19' = 'Dominican Republic';
                             '20' = 'Uruguay';
                             '21' = 'Nuevo Leon (Mexico)'")

serce <- serce %>%
  select(ses2, country, puntaje_estandar_final_lscore, puntaje_estandar_final_mscore,
         peso_estudiante_lscore, peso_estudiante_mscore) %>%
  filter(!is.na(ses2) & !is.na(country)) %>%
  group_by(country, ses2) %>%
  do(data.frame(score = quantile(.$puntaje_estandar_final_mscore, probs = seq(0, 100, 1), na.rm = T))) %>%
  ungroup(country, ses2) %>%
  mutate(rank = rep(0:100, length(unique(country)) * length(unique(ses2))),
         year = "2007",
         survey = "LLECE")
# 
# ##############
# 
############# PISA ############

# for (i in paste0("pbiecek/PISA", c(2012, 2009, 2006, 2003, 2000), "lite")) install_github(i, force = T)

library("PISA2000lite")
library("PISA2003lite")
library("PISA2006lite")
library("PISA2009lite")
library("PISA2012lite")
# For pisa 2000, CNT is COUNTRY

parent_edu <- "MISCED|FISCED"
edu_recode <- "1:3 = 1; 4:5 = 2; 6:7 = 3; else = NA"
cnt <- "CNT"

pisa_preparer <- function(df, parent_edu, edurecode, cnt) {
  
  # 
  # grab_changer <- function(df, vars_select, father_edu, mother_edu) {
  #   
  #   select_calls <- sapply(vars_select, function(col) {
  #     lazyeval::interp(~col_name, col_name = as.name(col))
  #   })
  #   
  #   fathers_moms_call <- sapply(c(father_edu, mother_edu), function(col){
  #     lazyeval::interp(~as.numeric(col_name), col_name = as.name(col))
  #   })
  #   
  #   
  #   df %>%
  #     select_(.dots = select_calls) %>%
  #     mutate_(.dots = setNames(fathers_moms_call, c("mom_isced", "dad_isced"))) %>%
  #     rename(country = CNT)
  #   
  # }
  # 
  # pisa_complete <- grab_changer(df, vars_select, father_edu, mother_edu)
  # 
  
  
  names(df)[grep(parent_edu, names(df))] <- c("dad_isced", "mom_isced")
  df[, "country"] <- df[, cnt]
  
  df$ses <- pmax(as.numeric(df$mom_isced), as.numeric(df$dad_isced), na.rm = T)
  df$ses2 <- car::recode(df$ses, edu_recode)
  # PQFISCED - father edu
  # 0 None
  # 1 ISCED 3A 
  # 2 ISCED 4 
  # 3 ISCED 5B 
  # 4 ISCED 5A, 6 
  # 9 Missing
  
  # PQMISCED - mother edu
  # 0 None
  # 1 ISCED 3A 
  # 2 ISCED 4 
  # 3 ISCED 5B 
  # 4 ISCED 5A, 6 
  # 9 Missing
  
  data <- df %>%
    filter(!is.na(country) & !is.na(ses2))
  
  pv_data <- pisa.per.pv("MATH", by = c("country", "ses2"), per = seq(0, 100, 1), data = data)

  pv_data
}

pisa2 <- lapply(c("math2000", paste0("student", seq(2003, 2012, by = 3))), function(x) {
  
  if (x == "math2000") cnt <- switch(cnt, "COUNTRY")
  df <- get(x)
  message("Data loaded correctly")
  data <- pisa_preparer(df, parent_edu, edu_recode)
  data$year <- gsub("[a-z]", "", x)
  data$country <- as.character(data$country)
  
  subset(data, select = c("country", "ses2", "Score", "year"))
})
pisa <- pisa2

###### country codes ########
country_codes <- readLines(textConnection(
  "Afghanistan	AF	AFG	004
  Aland Islands	AX	ALA	248
  Albania	AL	ALB	008
  Algeria	DZ	DZA	012
  American Samoa	AS	ASM	016
  Andorra	AD	AND	020
  Angola	AO	AGO	024
  Anguilla	AI	AIA	660
  Antarctica	AQ	ATA	010
  Antigua and Barbuda	AG	ATG	028
  Argentina	AR	ARG	032
  Armenia	AM	ARM	051
  Aruba	AW	ABW	533
  Australia	AU	AUS	036
  Austria	AT	AUT	040
  Azerbaijan	AZ	AZE	031
  Bahamas	BS	BHS	044
  Bahrain	BH	BHR	048
  Bangladesh	BD	BGD	050
  Barbados	BB	BRB	052
  Belarus	BY	BLR	112
  Belgium	BE	BEL	056
  Belize	BZ	BLZ	084
  Benin	BJ	BEN	204
  Bermuda	BM	BMU	060
  Bhutan	BT	BTN	064
  Bolivia	BO	BOL	068
  Bosnia and Herzegovina	BA	BIH	070
  Botswana	BW	BWA	072
  Bouvet Island	BV	BVT	074
  Brazil	BR	BRA	076
  British Virgin Islands	VG	VGB	092
  British Indian Ocean Territory	IO	IOT	086
  Brunei Darussalam	BN	BRN	096
  Bulgaria	BG	BGR	100
  Burkina Faso	BF	BFA	854
  Burundi	BI	BDI	108
  Cambodia	KH	KHM	116
  Cameroon	CM	CMR	120
  Canada	CA	CAN	124
  Cape Verde	CV	CPV	132
  Cayman Islands	KY	CYM	136
  Central African Republic	CF	CAF	140
  Chad	TD	TCD	148
  Chile	CL	CHL	152
  China	CN	CHN	156
  Hong Kong, Special Administrative Region of China	HK	HKG	344
  Macao, Special Administrative Region of China	MO	MAC	446
  Christmas Island	CX	CXR	162
  Cocos (Keeling) Islands	CC	CCK	166
  Colombia	CO	COL	170
  Comoros	KM	COM	174
  Congo (Brazzaville)	CG	COG	178
  Congo, Democratic Republic of the	CD	COD	180
  Cook Islands	CK	COK	184
  Costa Rica	CR	CRI	188
  Côte d'Ivoire	CI	CIV	384
  Croatia	HR	HRV	191
  Cuba	CU	CUB	192
  Cyprus	CY	CYP	196
  Czech Republic	CZ	CZE	203
  Denmark	DK	DNK	208
  Djibouti	DJ	DJI	262
  Dominica	DM	DMA	212
  Dominican Republic	DO	DOM	214
  Ecuador	EC	ECU	218
  Egypt	EG	EGY	818
  El Salvador	SV	SLV	222
  Equatorial Guinea	GQ	GNQ	226
  Eritrea	ER	ERI	232
  Estonia	EE	EST	233
  Ethiopia	ET	ETH	231
  Falkland Islands (Malvinas)	FK	FLK	238
  Faroe Islands	FO	FRO	234
  Fiji	FJ	FJI	242
  Finland	FI	FIN	246
  France	FR	FRA	250
  French Guiana	GF	GUF	254
  French Polynesia	PF	PYF	258
  French Southern Territories	TF	ATF	260
  Gabon	GA	GAB	266
  Gambia	GM	GMB	270
  Georgia	GE	GEO	268
  Germany	DE	DEU	276
  Ghana	GH	GHA	288
  Gibraltar	GI	GIB	292
  Greece	GR	GRC	300
  Greenland	GL	GRL	304
  Grenada	GD	GRD	308
  Guadeloupe	GP	GLP	312
  Guam	GU	GUM	316
  Guatemala	GT	GTM	320
  Guernsey	GG	GGY	831
  Guinea	GN	GIN	324
  Guinea-Bissau	GW	GNB	624
  Guyana	GY	GUY	328
  Haiti	HT	HTI	332
  Heard Island and Mcdonald Islands	HM	HMD	334
  Holy See (Vatican City State)	VA	VAT	336
  Honduras	HN	HND	340
  Hungary	HU	HUN	348
  Iceland	IS	ISL	352
  India	IN	IND	356
  Indonesia	ID	IDN	360
  Iran, Islamic Republic of	IR	IRN	364
  Iraq	IQ	IRQ	368
  Ireland	IE	IRL	372
  Isle of Man	IM	IMN	833
  Israel	IL	ISR	376
  Italy	IT	ITA	380
  Jamaica	JM	JAM	388
  Japan	JP	JPN	392
  Jersey	JE	JEY	832
  Jordan	JO	JOR	400
  Kazakhstan	KZ	KAZ	398
  Kenya	KE	KEN	404
  Kiribati	KI	KIR	296
  Korea, Democratic People's Republic of	KP	PRK	408
  Korea, Republic of	KR	KOR	410
  Kuwait	KW	KWT	414
  Kyrgyzstan	KG	KGZ	417
  Lao PDR	LA	LAO	418
  Latvia	LV	LVA	428
  Lebanon	LB	LBN	422
  Lesotho	LS	LSO	426
  Liberia	LR	LBR	430
  Libya	LY	LBY	434
  Liechtenstein	LI	LIE	438
  Lithuania	LT	LTU	440
  Luxembourg	LU	LUX	442
  Macedonia, Republic of	MK	MKD	807
  Madagascar	MG	MDG	450
  Malawi	MW	MWI	454
  Malaysia	MY	MYS	458
  Maldives	MV	MDV	462
  Mali	ML	MLI	466
  Malta	MT	MLT	470
  Marshall Islands	MH	MHL	584
  Martinique	MQ	MTQ	474
  Mauritania	MR	MRT	478
  Mauritius	MU	MUS	480
  Mayotte	YT	MYT	175
  Mexico	MX	MEX	484
  Micronesia, Federated States of	FM	FSM	583
  Moldova	MD	MDA	498
  Monaco	MC	MCO	492
  Mongolia	MN	MNG	496
  Montenegro	ME	MNE	499
  Montserrat	MS	MSR	500
  Morocco	MA	MAR	504
  Mozambique	MZ	MOZ	508
  Myanmar	MM	MMR	104
  Namibia	NA	NAM	516
  Nauru	NR	NRU	520
  Nepal	NP	NPL	524
  Netherlands	NL	NLD	528
  Netherlands Antilles	AN	ANT	530
  New Caledonia	NC	NCL	540
  New Zealand	NZ	NZL	554
  Nicaragua	NI	NIC	558
  Niger	NE	NER	562
  Nigeria	NG	NGA	566
  Niue	NU	NIU	570
  Norfolk Island	NF	NFK	574
  Northern Mariana Islands	MP	MNP	580
  Norway	NO	NOR	578
  Oman	OM	OMN	512
  Pakistan	PK	PAK	586
  Palau	PW	PLW	585
  Palestinian Territory, Occupied	PS	PSE	275
  Panama	PA	PAN	591
  Papua New Guinea	PG	PNG	598
  Paraguay	PY	PRY	600
  Peru	PE	PER	604
  Philippines	PH	PHL	608
  Pitcairn	PN	PCN	612
  Poland	PL	POL	616
  Portugal	PT	PRT	620
  Puerto Rico	PR	PRI	630
  Qatar	QA	QAT	634
  Réunion	RE	REU	638
  Romania	RO	ROU	642
  Russian Federation	RU	RUS	643
  Rwanda	RW	RWA	646
  Saint-Barthélemy	BL	BLM	652
  Saint Helena	SH	SHN	654
  Saint Kitts and Nevis	KN	KNA	659
  Saint Lucia	LC	LCA	662
  Saint-Martin (French part)	MF	MAF	663
  Saint Pierre and Miquelon	PM	SPM	666
  Saint Vincent and Grenadines	VC	VCT	670
  Samoa	WS	WSM	882
  San Marino	SM	SMR	674
  Sao Tome and Principe	ST	STP	678
  Saudi Arabia	SA	SAU	682
  Senegal	SN	SEN	686
  Serbia	RS	SRB	688
  Seychelles	SC	SYC	690
  Sierra Leone	SL	SLE	694
  Singapore	SG	SGP	702
  Slovakia	SK	SVK	703
  Slovenia	SI	SVN	705
  Solomon Islands	SB	SLB	090
  Somalia	SO	SOM	706
  South Africa	ZA	ZAF	710
  South Georgia and the South Sandwich Islands	GS	SGS	239
  South Sudan	SS	SSD	728
  Spain	ES	ESP	724
  Sri Lanka	LK	LKA	144
  Sudan	SD	SDN	736
  Suriname *	SR	SUR	740
  Svalbard and Jan Mayen Islands	SJ	SJM	744
  Swaziland	SZ	SWZ	748
  Sweden	SE	SWE	752
  Switzerland	CH	CHE	756
  Syrian Arab Republic (Syria)	SY	SYR	760
  Taiwan, Republic of China	TW	TWN	158
  Tajikistan	TJ	TJK	762
  Tanzania *, United Republic of	TZ	TZA	834
  Thailand	TH	THA	764
  Timor-Leste	TL	TLS	626
  Togo	TG	TGO	768
  Tokelau	TK	TKL	772
  Tonga	TO	TON	776
  Trinidad and Tobago	TT	TTO	780
  Tunisia	TN	TUN	788
  Turkey	TR	TUR	792
  Turkmenistan	TM	TKM	795
  Turks and Caicos Islands	TC	TCA	796
  Tuvalu	TV	TUV	798
  Uganda	UG	UGA	800
  Ukraine	UA	UKR	804
  United Arab Emirates	AE	ARE	784
  United Kingdom	GB	GBR	826
  United States of America	US	USA	840
  United States Minor Outlying Islands	UM	UMI	581
  Uruguay	UY	URY	858
  Uzbekistan	UZ	UZB	860
  Vanuatu	VU	VUT	548
  Venezuela (Bolivarian Republic of)	VE	VEN	862
  Viet Nam	VN	VNM	704
  Virgin Islands, US	VI	VIR	850
  Wallis and Futuna Islands	WF	WLF	876
  Western Sahara	EH	ESH	732
  Yemen	YE	YEM	887
  Zambia	ZM	ZMB	894
  Zimbabwe	ZW	ZWE	716
  Yugoslavia	00	YUG	000
  Chinese Taipei	00	TAP	000"
))
##########

country_codes <- trimws(country_codes)
names <- do.call(rbind, strsplit(country_codes, "\t"))[, c(1,3)]
country_search <- setNames(names[, 1], names[, 2])

pisa[2:3] <- lapply(pisa[2:3], function(x) {
  x$country <- country_search[x$country]
  x
})

capital <- function(vec) {
  vec <- tolower(vec)
  vec <- strsplit(vec, " ")
  
  cap_names <- sapply(vec, function(i) {
    paste0(paste0(toupper(substring(i, 1, 1)), substring(i, 2)), collapse = " ")
  })
  
  cap_names
}

pisa[[1]]$country <- capital(pisa[[1]]$country)

# PISA is ready. TERCE is still missing to be rbinded. SERCE is still lacking the country
# specification instead of the numbers. PIRLS and TIMMSS are still missing.

pisa_all <- do.call(rbind, pisa)
pisa_all$survey <- "PISA"

country_names <- c("Afghanistan", "Aland Islands", "Albania", "Algeria", "American Samoa",
                   "Andorra", "Angola", "Anguilla", "Antarctica", "Antigua", "Argentina",
                   "Armenia", "Aruba", "Australia", "Austria", "Azerbaijan", "Bahamas",
                   "Bahrain", "Bangladesh", "Barbados", "Barbuda", "Belarus", "Belgium",
                   "Belize", "Benin", "Bermuda", "Bhutan", "Bolivia", "Bosnia", "Botswana",
                   "Bouvet Island", "Brazil", "British Indian Ocean Trty.", "Brunei Darussalam",
                   "Bulgaria", "Burkina Faso", "Burundi", "Caicos Islands", "Cambodia",
                   "Cameroon", "Canada", "Cape Verde", "Cayman Islands", "Perm",
                   "Central African Republic", "Chad", "Chile", "Christmas Island",
                   "Cocos (Keeling) Islands", "Colombia", "Comoros", "Congo", "Miranda-Venezuela",
                   "Congo, Democratic Republic of the", "Cook Islands", "Costa Rica",
                   "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic",
                   "Denmark", "Djibouti", "Dominica", "Dominican Republic", "Ecuador",
                   "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia",
                   "Ethiopia", "Falkland Islands (Malvinas)", "Faroe Islands", "Fiji",
                   "Finland", "France", "French Guiana", "French Polynesia",
                   "French Southern Territories", "Futuna Islands", "Gabon",
                   "Gambia", "Georgia", "Germany", "Ghana", "Gibraltar", "Greece",
                   "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala",
                   "Guernsey", "Guinea", "Guinea-Bissau", "Guyana", "Haiti",
                   "Heard", "Herzegovina", "Holy See", "Honduras", "Hong Kong",
                   "Hungary", "Iceland", "Indonesia",
                   "Iran (Islamic Republic of)", "Iraq", "Ireland", "Isle of Man",
                   "Israel", "Italy", "Jamaica", "Jan Mayen Islands", "Japan",
                   "Jersey", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea",
                   "Korea (Democratic)", "Kuwait", "Kyrgyzstan", "Lao", "Latvia",
                   "Lebanon", "Lesotho", "Liberia", "Libyan Arab Jamahiriya",
                   "Liechtenstein", "Lithuania", "Luxembourg", "Macedonia",
                   "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali",
                   "Malta", "Marshall Islands", "Martinique", "Mauritania",
                   "Mauritius", "Mayotte", "McDonald Islands", "Mexico",
                   "Micronesia", "Miquelon", "Moldova", "Monaco", "Mongolia",
                   "Montenegro", "Montserrat", "Morocco", "Mozambique",
                   "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands",
                   "Netherlands Antilles", "Nevis", "New Caledonia",
                   "New Zealand", "Nicaragua", "Niger", "Nigeria",
                   "Niue", "Norfolk Island", "Northern Mariana Islands",
                   "Norway", "Oman", "Pakistan", "Palau",
                   "Palestinian Territory, Occupied", "Panama", "Papua New Guinea",
                   "Paraguay", "Peru", "Philippines", "Pitcairn", "Poland",
                   "Portugal", "Principe", "Puerto Rico", "Qatar", "Reunion",
                   "Romania", "Russian Federation", "Rwanda", "Saint Barthelemy",
                   "Saint Helena", "Saint Kitts", "Saint Lucia",
                   "Saint Martin (French part)", "Saint Pierre", "Saint Vincent",
                   "Samoa", "San Marino", "Sao Tome", "Saudi Arabia", "Senegal",
                   "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia",
                   "Slovenia", "Solomon Islands", "Somalia", "South Africa",
                   "South Georgia", "South Sandwich Islands", "Spain", "Sri Lanka",
                   "Sudan", "Suriname", "Svalbard", "Swaziland", "Sweden",
                   "Switzerland", "Syrian Arab Republic", "Taiwan", "Tajikistan",
                   "Tanzania", "Thailand", "The Grenadines", "Timor-Leste",
                   "Trinidad and Tobago", "Togo", "Tokelau", "Tonga", "Tunisia",
                   "Turkey", "Turkmenistan", "Turks Islands", "Tuvalu", "Uganda",
                   "Ukraine", "United Arab Emirates", "United Kingdom",
                   "United States", "Uruguay", "US Minor Outlying Islands",
                   "Uzbekistan", "Vanuatu", "Vatican City State",
                   "Vietnam", "Virgin Islands (British)", "Virgin Islands (US)",
                   "Wallis", "Western Sahara", "Yemen", "Zambia", "Zimbabwe",
                   "Macao", "Tamil Nadu-India",
                   "Himachal Pradesh-India", "China-Shanghai", "Chinese Taipei")


string_replacer <- function(pattern, replacement, vec) {
  vec[grep(pattern, vec)] <- replacement
  vec
}

for (i in seq_along(country_names)) pisa_all$country <- string_replacer(country_names[i],
                                                                        country_names[i],
                                                                        pisa_all$country)
pisa_all$country <- gsub("Honk", "Hong", pisa_all$country)
pisa_all$country <- gsub("Slovakia", "Slovak Republic", pisa_all$country)

pisa_all <- pisa_all %>%
  filter(!is.na(country)) %>%
  group_by(year, country, ses2) %>%
  mutate(rank = seq(0, 100, 1), score = Score) %>%
  ungroup(year, country, ses2) %>%
  select(-Score)

# pisa_all <- rbind(pisa_all, terce, serce)

pisa_all %>%
  filter(country == "Belgium", year == "2009") %>%
  mutate(ses2 = as.factor(ses2)) %>%
  ggplot(aes(score, rank, colour = as.factor(ses2))) +
  geom_point(alpha = 0.7, size = 2) +
  ylab("Student ranking") +
  xlab("Math test score") +
  ggtitle("Inequality for Belgium in 2009") +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_color_discrete(name = "Parent's education",
                       labels = c("Low educated",
                                  "Middle educated",
                                  "High educated")) +
  scale_alpha(guide = F) +
  annotate(geom = "text", x = 370, y = 60, label = "50th ranking", color = "#666666") +
  geom_segment(aes(x = 400, y = 58, xend = 438, yend = 50),
               arrow = arrow(length = unit(0.1, "cm")),
               color = "#666666") +
  theme_scientific() +
  theme(plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
        legend.position = c(0.85, 0.2),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        legend.text=element_text(size=14),
        legend.title = element_text(size=14))

ggsave("belgium.png", path = "/Users/cimentadaj/Downloads/inequality/shiny/www/")

# Fix the scaling of the score variable to make it comparable across surveys
# Find out how to properly measure the weighted quantiles or the summary score
# Merge with the PIRLS and TIMSS datasets.
# checkout why there's no variability in the quantiles of some countries.

#######################################

## TIMSS
# Download data from:
# http://rms.iea-dpc.org/

# Let's create the directories where all the TIMSS data is

tims_dir <- "/Users/cimentadaj/Downloads/TIMSS"
years <- as.character(c(1995, 1999, 2003, 2007, 2011)) # years available
grade <- rep(c("Grade 04", "Grade 08"), times = length(years)) # grades available
fl_year <- paste0("Y", years) # part of path

# Construct the path with all of the above and sort it so the first year is first
dirs <- sort(paste(paste(tims_dir, years, sep = "/"), grade, fl_year, "Data/SPSS/", sep = "/"))

dirs <- dirs[dir.exists(dirs)] # Check which path exists and only subset those which exist
                               # For example, 1999 only has 8th grade, so one path will be excluded.

years <- list("1995" = dirs[grep("1995", dirs)],
              '1999' = dirs[grep("1999", dirs)],
              '2003' = dirs[grep("2003", dirs)],
              '2007' = dirs[grep("2007", dirs)],
              '2011' = dirs[grep("2011", dirs)])

# function subsets recodes X number of variables with the same recode
subsetter <- function(df, vars, recode) {
  df[vars] <- lapply(df[vars], function(x) {
    car::recode(as.numeric(x), recode)
  })
  df
}

parent_edu_timss <- list("1995" = c("BSBGEDUM", "BSBGEDUF"),
                         "1999" = c("BSBGEDMO", "BSBGEDFA"),
                         "2003" = c("BSBGMFED", "BSBGFMED"),
                         "2007" = c("BS4GMFED", "BS4GFMED"),
                         "2011" = c("BSBG06A", "BSBG06B"))

all_codings_timss <- c("0:2 = 1; 3:4 = 2; 5:6 = 3; 7 = NA",
                       "0:3 = 1; 4:5 = 2; 6:7 = 3; 8 = NA",
                       "0:3 = 1; 4:5 = 2; 6:8 = 3; 9 = NA",
                       "0:2 = 1; 3:4 = 2; 5:7 = 3; 8 = NA",
                       "0:2 = 1; 3:4 = 2; 5:7 = 3; 8 = NA")

student_wrangler <- function(directories, parents_edu, codings, survey) {
  stopifnot(!missing(survey))
  
  if (survey == "TIMSS") {
    
    list_df <- lapply(1:length(directories), function(i) {
      dat <- timssg8.select.merge(directories[i], student = parents_edu[[i]])
      dat <- subsetter(dat, parents_edu[[i]], codings[i])
      dat$ses2 <- pmax(dat[, parents_edu[[i]][1]], dat[, parents_edu[[i]][2]], na.rm = T)
      dat
      
    })
  }
  
  
  if (survey == "PIRLS") {
    
  list_df <- lapply(1:length(directories), function(i) {
    dat <- pirls.select.merge(directories[i], home = parents_edu[[i]])
    dat <- subsetter(dat, parents_edu[[i]], codings[i])
    dat$ses2 <- pmax(dat[, parents_edu[[i]][1]], dat[, parents_edu[[i]][2]], na.rm = T)
    dat
    
    })
  }
  
  list_df
}

timss_list2 <- setNames(student_wrangler(dirs[grep("08", dirs)],
                                     parent_edu_timss,
                                     all_codings_timss,
                                     survey = "TIMSS"),
                        c("1995", "1999", "2003", "2007", "2011"))

# See here for education coding:
# http://ec.europa.eu/eurostat/documents/1978984/6037342/Comparability_ISCED_2011_ISCED_1997.pdf

# Coding for 1995
# g4_95 - parent's education wasn't asked

# 1 =  <finished primary school>
# 2 =  <finished some secondary school>
# 3 =  <finished secondary school>
# 4 =  <some vocational/technical education after secondary school>
# 5 =  <some university>
# 6 =  <finished university>
# 7 =  I don’t know

# Coding for 1999
# g4_99 - parent's education wasn't asked

# 1 = <some primary school, or did not go to school>
# 2 = <finished primary school>
# 3 = <some secondary school>
# 4 = <finished secondary school>
# 5 = <some vocational/technical education after secondary school>
# 6 = <some university>
# 7 = <finished university>
# 8 = <I don’t know>

# Coding for 2003
# g4_03 - parent's education wasn't asked

# 1 = Did not finish <ISCED 1> or did not go to school
# 2 = <ISCED 1>
# 3 = <ISCED 2>
# 4 = <ISCED 3>
# 5 = <ISCED 4B>
# 6 = <ISCED 5B>
# 7 = <ISCED 5A, first degree>
# 8 = Beyond <ISCED 5A, first degree>
# 9 = I don't know

# Coding for 2007
# g4_07 - parent's education wasn't asked

# 1 = Some <ISCED Level 1 or 2> or did not go to school                           
# 2 = <ISCED 2>
# 3 = <ISCED 3>
# 4 = <ISCED 4>
# 5 = <ISCED 5B>
# 6 = <ISCED 5A,  rst degree>
# 7 = <Beyond <ISCED 5A,  rst degree>
# 8 = I don’t know

# Coding for 2011
# g4_11 <- timssg4.select.merge(folder = years[[5]][1],home = c("ASBH17A", "ASBH17B"))

# 1 = Some <ISCED Level 1 or 2 > or did not go to school
# 2 = <ISCED Level 2>
# 3 = <ISCED Level 3>
# 4 = <ISCED Level 4>
# 5 = <ISCED Level 5B>
# 6 = <ISCED Level 5A, first degree>
# 7 = Beyond <ISCED Level 5A, first degree>
# 8 = I don’t know

timss_list <- lapply(1:length(timss_list), function(num) {
  message("Data loaded correctly")
  x <- timss_list[[num]]
  x$country <- as.character(x$IDCNTRYL)
  x$grade <- 8
  x <- subset(x, !is.na(ses2))
  x$year <- names(timss_list)[num]
  timss.per.pv(by = c("year", "country", "ses2", "grade"), per = seq(0, 100, 1), data = x)
})

timss <- do.call(rbind, timss_list)[c("country", "ses2", "Score", "year")]

timss <- timss %>%
  group_by(year, country, ses2) %>%
  mutate(survey = "TIMSS", rank = seq(1, 100, 1)) %>%
  ungroup(year, country, ses2) %>%
  mutate(country = as.character(country),
         year = as.character(year),
         score = Score) %>%
  select(-Score)


############################

# PIRLS

pirls_dir <- "/Users/cimentadaj/Downloads/PIRLS/"
years <- c("2001", "2006", "2011")
pirls_dir2 <- paste0(pirls_dir, years, "/PIRLS/Grade 04/", "Y", years, "/Data/SPSS/")


eduparent_list <- list("2001" = c("ASBHEDUF", "ASBHEDUM"),
                       "2006" = c("ASBHLEDF", "ASBHLEDM"),
                       "2011" = c("ASBH17A", "ASBH17B"))

all_codings <- c("0:2 = 1; 3:6 = 2; 7:8 = 3; 9 = NA",
                 "0:2 = 1; 3:4 = 2; 5:7 = 3; 8 = NA",
                 "0:3 = 1; 4:5 = 2; 6:8 = 3; 9 = NA")

pirls_list2 <- setNames(student_wrangler(pirls_dir2,
                                         eduparent_list,
                                         all_codings,
                                         survey = "PIRLS"),
                        c("2001", "2006", "2011"))

# Just in case:

# 2001 coding
# 1 = Some <ISCED Level 1 or 2> or did not go to school
# 2 = <ISCED Level 2>
# 3 = <ISCED Level 3A or 3B>
# 4 = <ISCED Level 3C>
# 5 = <ISCED Level 4A>
# 6 = <ISCED Level 4B>
# 7 = <ISCED Level 5A> or higher
# 8 = < ISCED Level 5B> or higher
# 9 = Notapplicable

# 2006 coding
# 1 = Some <ISCED Level 1 or 2> or did not go to school
# 2 = <ISCED Level 2>
# 3 = <ISCED Level 3>
# 4 = <ISCED Level 4>
# 5 = <ISCED Level 5B>
# 6 = <ISCED Level 5A, first degree>
# 7 = Beyond <ISCED Level 5A, first degree>
# 8 = Not applicable

# 2011 coding
# 1 = Did not go to school
# 2 = Some <ISCED Level 1 or 2>
# 3 = <ISCED Level 2>
# 4 = <ISCED Level 3>
# 5 = <ISCED Level 4>
# 6 = <ISCED Level 5B>
# 7 = <ISCED Level 5A, first degree>
# 8 = Beyond <ISCED Level 5A, A first degree>
# 9 = Not applicable

pirls_list <- lapply(1:length(pirls_list), function(num) {
  message("Data loaded correctly")
  x <- pirls_list[[num]]
  x$country <- as.character(x$IDCNTRYL)
  x$grade <- 4
  x <- subset(x, !is.na(ses2))
  x$year <- names(pirls_list[num])
  pirls.per.pv(by = c("year", "country", "ses2", "grade"), per = seq(0, 100, 1), data = x)
})

pirls <- do.call(rbind, pirls_list)[c("country", "ses2", "Score", "year", "Percentiles")]

pirls <- pirls %>%
  group_by(year, country, ses2) %>%
  mutate(survey = "PIRLS") %>%
  rename(rank = Percentiles) %>%
  ungroup(year, country, ses2) %>%
  mutate(country = as.character(country),
         year = as.character(year),
         score = Score) %>%
  select(-Score)

##################

## PIAAC
install_github("pbiecek/PIAAC", force = T)
library(PIAAC)


##################
# Saving
write_csv(rbind(pisa_all, timss, pirls),
          "/Users/cimentadaj/Downloads/inequality/shiny/pisa.csv")

