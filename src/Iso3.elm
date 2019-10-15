module Iso3 exposing (Iso3Record, NationIso3, iso3Codes)


type alias NationIso3 =
    String


type alias Iso3Record =
    { countryOrArea : String, m49Code : Int, iso3Code : NationIso3 }


iso3Codes : List Iso3Record
iso3Codes =
    codesTxtSource |> String.split "\n" |> List.map parseRow


parseRow : String -> Iso3Record
parseRow row =
    let
        default =
            Iso3Record "Albania" 8 "ALB"

        toRecord : List String -> Iso3Record
        toRecord xs =
            case xs of
                country :: m49 :: iso3 :: [] ->
                    m49
                        |> String.toInt
                        |> Maybe.map (\m49Code -> Iso3Record country m49Code iso3)
                        |> Maybe.withDefault default

                _ ->
                    default
    in
    row |> String.split "\t" |> toRecord


codesTxtSource =
    """Afghanistan\t004\tAFG
Åland Islands\t248\tALA
Albania\t008\tALB
Algeria\t012\tDZA
American Samoa\t016\tASM
Andorra\t020\tAND
Angola\t024\tAGO
Anguilla\t660\tAIA
Antarctica\t010\tATA
Antigua and Barbuda\t028\tATG
Argentina\t032\tARG
Armenia\t051\tARM
Aruba\t533\tABW
Australia\t036\tAUS
Austria\t040\tAUT
Azerbaijan\t031\tAZE
Bahamas\t044\tBHS
Bahrain\t048\tBHR
Bangladesh\t050\tBGD
Barbados\t052\tBRB
Belarus\t112\tBLR
Belgium\t056\tBEL
Belize\t084\tBLZ
Benin\t204\tBEN
Bermuda\t060\tBMU
Bhutan\t064\tBTN
Bolivia (Plurinational State of)\t068\tBOL
Bonaire, Sint Eustatius and Saba\t535\tBES
Bosnia and Herzegovina\t070\tBIH
Botswana\t072\tBWA
Bouvet Island\t074\tBVT
Brazil\t076\tBRA
British Indian Ocean Territory\t086\tIOT
British Virgin Islands\t092\tVGB
Brunei Darussalam\t096\tBRN
Bulgaria\t100\tBGR
Burkina Faso\t854\tBFA
Burundi\t108\tBDI
Cabo Verde\t132\tCPV
Cambodia\t116\tKHM
Cameroon\t120\tCMR
Canada\t124\tCAN
Cayman Islands\t136\tCYM
Central African Republic\t140\tCAF
Chad\t148\tTCD
Chile\t152\tCHL
China\t156\tCHN
China, Hong Kong Special Administrative Region\t344\tHKG
China, Macao Special Administrative Region\t446\tMAC
Christmas Island\t162\tCXR
Cocos (Keeling) Islands\t166\tCCK
Colombia\t170\tCOL
Comoros\t174\tCOM
Congo\t178\tCOG
Cook Islands\t184\tCOK
Costa Rica\t188\tCRI
Côte d’Ivoire\t384\tCIV
Croatia\t191\tHRV
Cuba\t192\tCUB
Curaçao\t531\tCUW
Cyprus\t196\tCYP
Czechia\t203\tCZE
Democratic People's Republic of Korea\t408\tPRK
Democratic Republic of the Congo\t180\tCOD
Denmark\t208\tDNK
Djibouti\t262\tDJI
Dominica\t212\tDMA
Dominican Republic\t214\tDOM
Ecuador\t218\tECU
Egypt\t818\tEGY
El Salvador\t222\tSLV
Equatorial Guinea\t226\tGNQ
Eritrea\t232\tERI
Estonia\t233\tEST
Eswatini\t748\tSWZ
Ethiopia\t231\tETH
Falkland Islands (Malvinas)\t238\tFLK
Faroe Islands\t234\tFRO
Fiji\t242\tFJI
Finland\t246\tFIN
France\t250\tFRA
French Guiana\t254\tGUF
French Polynesia\t258\tPYF
French Southern Territories\t260\tATF
Gabon\t266\tGAB
Gambia\t270\tGMB
Georgia\t268\tGEO
Germany\t276\tDEU
Ghana\t288\tGHA
Gibraltar\t292\tGIB
Greece\t300\tGRC
Greenland\t304\tGRL
Grenada\t308\tGRD
Guadeloupe\t312\tGLP
Guam\t316\tGUM
Guatemala\t320\tGTM
Guernsey\t831\tGGY
Guinea\t324\tGIN
Guinea-Bissau\t624\tGNB
Guyana\t328\tGUY
Haiti\t332\tHTI
Heard Island and McDonald Islands\t334\tHMD
Holy See\t336\tVAT
Honduras\t340\tHND
Hungary\t348\tHUN
Iceland\t352\tISL
India\t356\tIND
Indonesia\t360\tIDN
Iran (Islamic Republic of)\t364\tIRN
Iraq\t368\tIRQ
Ireland\t372\tIRL
Isle of Man\t833\tIMN
Israel\t376\tISR
Italy\t380\tITA
Jamaica\t388\tJAM
Japan\t392\tJPN
Jersey\t832\tJEY
Jordan\t400\tJOR
Kazakhstan\t398\tKAZ
Kenya\t404\tKEN
Kiribati\t296\tKIR
Kuwait\t414\tKWT
Kyrgyzstan\t417\tKGZ
Lao People's Democratic Republic\t418\tLAO
Latvia\t428\tLVA
Lebanon\t422\tLBN
Lesotho\t426\tLSO
Liberia\t430\tLBR
Libya\t434\tLBY
Liechtenstein\t438\tLIE
Lithuania\t440\tLTU
Luxembourg\t442\tLUX
Madagascar\t450\tMDG
Malawi\t454\tMWI
Malaysia\t458\tMYS
Maldives\t462\tMDV
Mali\t466\tMLI
Malta\t470\tMLT
Marshall Islands\t584\tMHL
Martinique\t474\tMTQ
Mauritania\t478\tMRT
Mauritius\t480\tMUS
Mayotte\t175\tMYT
Mexico\t484\tMEX
Micronesia (Federated States of)\t583\tFSM
Monaco\t492\tMCO
Mongolia\t496\tMNG
Montenegro\t499\tMNE
Montserrat\t500\tMSR
Morocco\t504\tMAR
Mozambique\t508\tMOZ
Myanmar\t104\tMMR
Namibia\t516\tNAM
Nauru\t520\tNRU
Nepal\t524\tNPL
Netherlands\t528\tNLD
New Caledonia\t540\tNCL
New Zealand\t554\tNZL
Nicaragua\t558\tNIC
Niger\t562\tNER
Nigeria\t566\tNGA
Niue\t570\tNIU
Norfolk Island\t574\tNFK
North Macedonia\t807\tMKD
Northern Mariana Islands\t580\tMNP
Norway\t578\tNOR
Oman\t512\tOMN
Pakistan\t586\tPAK
Palau\t585\tPLW
Panama\t591\tPAN
Papua New Guinea\t598\tPNG
Paraguay\t600\tPRY
Peru\t604\tPER
Philippines\t608\tPHL
Pitcairn\t612\tPCN
Poland\t616\tPOL
Portugal\t620\tPRT
Puerto Rico\t630\tPRI
Qatar\t634\tQAT
Republic of Korea\t410\tKOR
Republic of Moldova\t498\tMDA
Réunion\t638\tREU
Romania\t642\tROU
Russian Federation\t643\tRUS
Rwanda\t646\tRWA
Saint Barthélemy\t652\tBLM
Saint Helena\t654\tSHN
Saint Kitts and Nevis\t659\tKNA
Saint Lucia\t662\tLCA
Saint Martin (French Part)\t663\tMAF
Saint Pierre and Miquelon\t666\tSPM
Saint Vincent and the Grenadines\t670\tVCT
Samoa\t882\tWSM
San Marino\t674\tSMR
Sao Tome and Principe\t678\tSTP
Sark\t680\t
Saudi Arabia\t682\tSAU
Senegal\t686\tSEN
Serbia\t688\tSRB
Seychelles\t690\tSYC
Sierra Leone\t694\tSLE
Singapore\t702\tSGP
Sint Maarten (Dutch part)\t534\tSXM
Slovakia\t703\tSVK
Slovenia\t705\tSVN
Solomon Islands\t090\tSLB
Somalia\t706\tSOM
South Africa\t710\tZAF
South Georgia and the South Sandwich Islands\t239\tSGS
South Sudan\t728\tSSD
Spain\t724\tESP
Sri Lanka\t144\tLKA
State of Palestine\t275\tPSE
Sudan\t729\tSDN
Suriname\t740\tSUR
Svalbard and Jan Mayen Islands\t744\tSJM
Sweden\t752\tSWE
Switzerland\t756\tCHE
Syrian Arab Republic\t760\tSYR
Tajikistan\t762\tTJK
Thailand\t764\tTHA
Timor-Leste\t626\tTLS
Togo\t768\tTGO
Tokelau\t772\tTKL
Tonga\t776\tTON
Trinidad and Tobago\t780\tTTO
Tunisia\t788\tTUN
Turkey\t792\tTUR
Turkmenistan\t795\tTKM
Turks and Caicos Islands\t796\tTCA
Tuvalu\t798\tTUV
Uganda\t800\tUGA
Ukraine\t804\tUKR
United Arab Emirates\t784\tARE
United Kingdom of Great Britain and Northern Ireland\t826\tGBR
United Republic of Tanzania\t834\tTZA
United States Minor Outlying Islands\t581\tUMI
United States of America\t840\tUSA
United States Virgin Islands\t850\tVIR
Uruguay\t858\tURY
Uzbekistan\t860\tUZB
Vanuatu\t548\tVUT
Venezuela (Bolivarian Republic of)\t862\tVEN
Viet Nam\t704\tVNM
Wallis and Futuna Islands\t876\tWLF
Western Sahara\t732\tESH
Yemen\t887\tYEM
Zambia\t894\tZMB
Zimbabwe\t716\tZWE"""
