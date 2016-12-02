# crude analysis of the correlation between average country performance and the average difference between private and public schools
# everything is really dirty but there seems to be no correlation between the two, quite surprisingly.

library(tidyverse)
library(downloader)
library(car)
library(Hmisc)
library(artyfarty)


###### 2013 ###########
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
                             'NLE' = 'Nuevo Leon';
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
  do(data.frame(score = wtd.quantile(.$puntaje_estandar_math, weights = .$wgm_math, probs = seq(0, 1, 0.01)))) %>%
  ungroup(country, ses2) %>%
  mutate(rank = rep(0:100, length(unique(country)) * length(unique(ses2))),
         year = "2013")

###############

##### 2007 #######
# load serce function

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
serce$year <- "2006"

serce <- serce %>%
  select(ses2, country, puntaje_estandar_final_lscore, puntaje_estandar_final_mscore,
         peso_estudiante_lscore, peso_estudiante_mscore) %>%
  filter(!is.na(ses2) & !is.na(country)) %>%
  group_by(country, ses2) %>%
  do(data.frame(score = wtd.quantile(.$puntaje_estandar_final_lscore, weights = .$peso_estudiante_lscore, probs = seq(0, 1, 0.01)))) %>%
  ungroup(country, ses2) %>%
  mutate(rank = rep(0:100, length(unique(country)) * length(unique(ses2)))
         year = "2007")

##############

############# PISA ############

# for (i in paste0("pbiecek/PISA", c(2012, 2009, 2006, 2003, 2000), "lite")) install_github(i, force = T)

library("PISA2000lite")
library("PISA2003lite")
library("PISA2006lite")
library("PISA2009lite")
library("PISA2012lite")
# For pisa 2000, CNT is COUNTRY and PV1SCIE is not in the math data

vars_select <- c("CNT", "FISCED", "MISCED", "PV1MATH", "PV1READ", "W_FSTUWT")
mother_edu <- "MISCED"
father_edu <- "FISCED"
edu_recode <- "1:3 = 1; 4:5 = 2; 6:7 = 3; else = NA"

pisa_preparer <- function(df, vars_select, father_edu, mother_edu, edu_recode) {
  
  
  grab_changer <- function(df, vars_select, father_edu, mother_edu) {
    
    select_calls <- sapply(vars_select, function(col) {
      lazyeval::interp(~col_name, col_name = as.name(col))
    })
    
    fathers_moms_call <- sapply(c(father_edu, mother_edu), function(col){
      lazyeval::interp(~as.numeric(col_name), col_name = as.name(col))
    })
    
    
    df %>%
      select_(.dots = select_calls) %>%
      mutate_(.dots = setNames(fathers_moms_call, c("mom_isced", "dad_isced"))) %>%
      rename(country = CNT)
    
  }
  
  pisa_complete <- grab_changer(df, vars_select, father_edu, mother_edu)
  
  
  
  big <- function (data, var1, var2) {
    attach(data)
    out <- ifelse(is.na(var1) & is.na(var2), NA,
                  ifelse(is.na(var1) & !is.na(var2), var2,
                         ifelse(!is.na(var1) & is.na(var2), var1,
                                ifelse(var1 > var2, var1, var2))))
    detach(data)
    out
  }
  
  pisa_complete$ses <- big(pisa_complete, mom_isced, dad_isced)
  pisa_complete$ses2 <- car::recode(pisa_complete$ses, eval(edu_recode))
  names(pisa_complete) <- tolower(names(pisa_complete))
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
  
  
  data <- pisa_complete %>%
    filter(!is.na(country) & !is.na(ses2)) %>%
    group_by(country, ses2) %>%
    do(data.frame(score = wtd.quantile(.$pv1math, weights = .$w_fstuwt, probs = seq(0, 1, 0.01)))) %>%
    ungroup(country, ses2) %>%
    mutate(rank = rep(0:100, length(unique(country))*length(unique(ses2))))
  
  data
}

pisa <- lapply(c("math2000", paste0("student", seq(2003, 2012, by = 3))), function(x) {
  
  if (x == "mathh2000") vars_select[1] <- switch(vars_select[1], "COUNTRY")
  df <- get(x)
  message("Data loaded correctly")
  data <- pisa_preparer(df, vars_select, father_edu, mother_edu, edu_recode)
  data$year <- gsub("[a-z]", "", x)
  data$country <- as.character(data$country)
  data
})

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

pisa_all %>%
  filter(country == "Belgium", year == "2009") %>%
  mutate(ses2 = as.factor(ses2)) %>%
  ggplot(aes(score, rank, colour = as.factor(ses2), alpha = 0.05)) +
  geom_point() +
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
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.85, 0.2))

ggsave("belgium.png", device = "png", path = "/Users/cimentadaj/Downloads/inequality/www/")
