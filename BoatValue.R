# These are libaries with functions used here. Make sure that the libraries are
# installed before you run the code or you will get an error
library(ggplot2)
library (stringr)
library (lubridate)
library (nlme)
library (rms)
library (boot)
library (ggmap)
library (bootstrap)
library (MASS)
library (glmulti)
library (reshape2)
library (xtable)
library (Hmisc)

# Read the file, make sure that the file is on csv format and in the same folder
# than this script. The easiest is if you use the same structure (dont add or
# delete columns on the file). Also put new data on that file
VesselLog <- read.csv ("Working Vessel Log 2013-11-10.csv")
# Keep only uselful columns A.K.A discard shit we don't need
VesselLog <- VesselLog[,c(1, 2, 6, 7, 8, 9, 10, 11, 13, 16)]

# Delete empty rows
VesselLog <- VesselLog[VesselLog$Date != "", ]

# This section will fill empty times and funny ones with 00:00 and interpret dates
VesselLog$Time <- as.character (VesselLog$Time)
# If the cell is empty fill it with midnight
VesselLog$Time[VesselLog$Time == ""] <- "00:00"
# Combine date and time in one and tell R that its a date from this timezone
VesselLog$DateTime <- as.POSIXct(paste(VesselLog$Date, VesselLog$Time), format = "%d-%b-%y %H:%M", tz = "Asia/Thimphu")
# Some times are bad. Fill them with midnight as well and repeat the thing
VesselLog$Time[is.na(VesselLog$DateTime)] <- "00:00"
VesselLog$DateTime <- as.POSIXct(paste(VesselLog$Date, VesselLog$Time), format = "%d-%b-%y %H:%M", tz = "Asia/Thimphu")
# Delete the second column (with times). This keep our data frame tidy
VesselLog <- VesselLog[, -2]

# Clean up the PeopleonBoard to represent numbers
VesselLog$PeopleonBoard[VesselLog$PeopleonBoard == "at least 1"] <- "1"
VesselLog$PeopleonBoard[VesselLog$PeopleonBoard == "at least 7"] <- "7"
VesselLog$PeopleonBoard[VesselLog$PeopleonBoard == "2-Mar"] <- "1"
VesselLog$PeopleonBoard[VesselLog$PeopleonBoard == "N/A"] <- "1"
VesselLog$PeopleonBoard <- as.numeric(VesselLog$PeopleonBoard)

# Correct Lattitudes and Longitudes
VesselLog$Lattitude <- as.numeric(as.character(VesselLog$Lattitude))
VesselLog$Longitude <- as.numeric(as.character(VesselLog$Longitude))
VesselLog$Lattitude[VesselLog$Lattitude > 3.6] <- NA
VesselLog$Lattitude[VesselLog$Lattitude < 3.44] <- NA
VesselLog$Longitude[VesselLog$Longitude > 72.95] <- NA
VesselLog$Longitude[VesselLog$Longitude < 72.7] <- NA


# Correct Vessel Types God Damn Shit!
VesselLog$Type <- as.character(VesselLog$Type)
VesselLog$Type[VesselLog$Type == ""] <- "Other"
VesselLog$Type[VesselLog$Type == "Ferry"] <- "Other"
VesselLog$Type[VesselLog$Type == "JET SKI"] <- "Other"
VesselLog$Type[VesselLog$Type == "Jet Skis"] <- "Other"
VesselLog$Type[VesselLog$Type == "jetski"] <- "Other"
VesselLog$Type[VesselLog$Type == "Jetski"] <- "Other"
VesselLog$Type[VesselLog$Type == "LF B"] <- "LFV"
VesselLog$Type[VesselLog$Type == "LFB"] <- "LFV"
VesselLog$Type[VesselLog$Type == "Local fishing vessel"] <- "LFV"
VesselLog$Type[VesselLog$Type == "LVB (SPEED BOAT)"] <- "LVB"
VesselLog$Type[VesselLog$Type == "LVB "] <- "LVB"
VesselLog$Type[VesselLog$Type == "LVB0"] <- "LVB"
VesselLog$Type[VesselLog$Type == "LVBB"] <- "LVB"
VesselLog$Type[VesselLog$Type == "LVD"] <- "LVB"
VesselLog$Type[VesselLog$Type == "LAV"] <- "LVB"
VesselLog$Type[VesselLog$Type == "LVR"] <- "LVB"
VesselLog$Type[VesselLog$Type == "N/A"] <- "Other"
VesselLog$Type[VesselLog$Type == "RFB"] <- "RSFB"
VesselLog$Type[VesselLog$Type == "RFV"] <- "RSFB"
VesselLog$Type[VesselLog$Type == "RSFV"] <- "RSFB"
VesselLog$Type[VesselLog$Type == "SRFV"] <- "RSFB"
VesselLog$Type[VesselLog$Type == "RSD"] <- "RSB"
VesselLog$Type[VesselLog$Type == "RJB"] <- "RSB"
VesselLog$Type[VesselLog$Type == "RSB "] <- "RSB"
VesselLog$Type[VesselLog$Type == "RSP "] <- "RSB"
VesselLog$Type[VesselLog$Type == "RSP"] <- "RSB"
VesselLog$Type[VesselLog$Type == "RSV"] <- "RSB"
VesselLog$Type[VesselLog$Type == "Sail"] <- "RED"
VesselLog$Type[VesselLog$Type == "REB"] <- "RED"
VesselLog$Type[VesselLog$Type == "Sailboat"] <- "RED"
VesselLog$Type[VesselLog$Type == "SB"] <- "RED"
VesselLog$Type[VesselLog$Type == "tender"] <- "Tender"
VesselLog$Type[VesselLog$Type == "Tender x2"] <- "Tender"
VesselLog$Type[VesselLog$Type == "Yacht"] <- "RED"
VesselLog$Type[VesselLog$Type == "SAIL"] <- "RED"
VesselLog$Type[VesselLog$Type == "Sail boat"] <- "RED"
VesselLog$Type[VesselLog$Type == "REV"] <- "RED"
VesselLog$Type[VesselLog$Type == "LBBDD"] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LBDD"] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LVDD"] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LBVDD"] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LPDD"] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LUBB"] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LVBD"] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LVBDD "] <- "LVBDD"
VesselLog$Type[VesselLog$Type == "LVDD "] <- "LVBDD"
VesselLog$Type <- as.factor(VesselLog$Type)

VesselLog$Association <- NA
VesselLog$Association[VesselLog$Type == "LFV"] <- "Local"
VesselLog$Association[VesselLog$Type == "LVB"] <- "Liveaboard"
VesselLog$Association[VesselLog$Type == "LVBDD"] <- "Liveaboard"
VesselLog$Association[VesselLog$Type == "Other"] <- "Other"
VesselLog$Association[VesselLog$Type == "RDD"] <- "Resort"
VesselLog$Association[VesselLog$Type == "RED"] <- "Resort"
VesselLog$Association[VesselLog$Type == "RSB"] <- "Resort"
VesselLog$Association[VesselLog$Type == "RSFB"] <- "Resort"
VesselLog$Association[VesselLog$Type == "Tender"] <- "Liveaboard"
VesselLog$Association <- as.factor(VesselLog$Association)

# Remove price from those with average (later we will calculate new averages)
VesselLog$Notes.1 [VesselLog$Notes.1 == "Put average"] <- "Put Average"
VesselLog$UnitPrice[VesselLog$Notes.1 == "Put Average"] <- NA

# Correct Vessel Names God Damn Shit!
VesselLog$Vname <- as.character (VesselLog$Vname)
VesselLog$Vname[VesselLog$Vname == ""] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "873817-06 Kandooma Aqua" ] <- "Kandooma"
VesselLog$Vname[VesselLog$Vname == "972804" ] <- "Name Unseen"
VesselLog$Vname[VesselLog$Vname == "Aaru" ] <- "Aary"
VesselLog$Vname[VesselLog$Vname == "Abyssworld" ] <- "Abyssworld.com"
VesselLog$Vname[VesselLog$Vname == "AbyssWorld.com" ] <- "Abyssworld.com"
VesselLog$Vname[VesselLog$Vname == "Adventurer 2" ] <- "Adventure 2"
VesselLog$Vname[VesselLog$Vname == "Amaz" ] <- "Amaaz 2"
VesselLog$Vname[VesselLog$Vname == "Amaz 2" ] <- "Amaaz 2"
VesselLog$Vname[VesselLog$Vname == "Amazz 2" ] <- "Amaaz 2"
VesselLog$Vname[VesselLog$Vname == "Ammaz-2" ] <- "Amaaz 2"
VesselLog$Vname[VesselLog$Vname == "Angaga Torundo" ] <- "Angaga"
VesselLog$Vname[VesselLog$Vname == "Araamagy" ] <- "Aramagu"
VesselLog$Vname[VesselLog$Vname == "Ari queen" ] <- "Ari Queen"
VesselLog$Vname[VesselLog$Vname == "Arona" ] <- "Aroma"
VesselLog$Vname[VesselLog$Vname == "Aruna" ] <- "Aroma"
VesselLog$Vname[VesselLog$Vname == "Atoll Challenge" ] <- "Atoll Challenger"
VesselLog$Vname[VesselLog$Vname == "Ark Royal Dive Dhoni" ] <- "Ark Royal"
VesselLog$Vname[VesselLog$Vname == "Atoll Dive" ] <- "Atoll Diver"
VesselLog$Vname[VesselLog$Vname == "Attavamas" ] <- "Attamas"
VesselLog$Vname[VesselLog$Vname == "Avevara" ] <- "Arevara"
VesselLog$Vname[VesselLog$Vname == "Banyan 12" ] <- "Banyan"
VesselLog$Vname[VesselLog$Vname == "Banyan Tree Maldivian 2" ] <- "Banyan"
VesselLog$Vname[VesselLog$Vname == "Bayan Tree Maldiviarv2" ] <- "Banyan"
VesselLog$Vname[VesselLog$Vname == "Belog to Kafi" ] <- "Kefi"
VesselLog$Vname[VesselLog$Vname == "Blue shark 2" ] <- "Blue Shark"
VesselLog$Vname[VesselLog$Vname == "Blue Shark 2" ] <- "Blue Shark"
VesselLog$Vname[VesselLog$Vname == "Blue wave" ] <- "Blue Wave"
VesselLog$Vname[VesselLog$Vname == "C12880-01" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "C153" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "C9702404" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "C9702A04 ???" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Canopos Maldives" ] <- "CanopusMaldives.com"
VesselLog$Vname[VesselLog$Vname == "Canopus Maldives" ] <- "CanopusMaldives.com"
VesselLog$Vname[VesselLog$Vname == "CanupusMaldives.com" ] <- "CanopusMaldives.com"
VesselLog$Vname[VesselLog$Vname == "Cape Diem" ] <- "Carpe Diem"
VesselLog$Vname[VesselLog$Vname == "Cape diem" ] <- "Carpe Diem"
VesselLog$Vname[VesselLog$Vname == "Carpe diem" ] <- "Carpe Diem"
VesselLog$Vname[VesselLog$Vname == "Carpe Vita " ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Carpe Vita" ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Coupe Vitta " ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Coupe Vita " ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Carpe Vita Explora" ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Centara Grand" ] <- "Centara"
VesselLog$Vname[VesselLog$Vname == "CONRAD" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad " ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad (katie)" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad boat" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad Boat" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad Manta 2" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad Private Trip" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad speed boat" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad WS Day Trip" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Carina" ] <- "Corina"
VesselLog$Vname[VesselLog$Vname == "Corina Dive" ] <- "Corina"
VesselLog$Vname[str_detect(VesselLog$Vname, "Vila")] <- "Villa Dhuveli"
VesselLog$Vname[str_detect(VesselLog$Vname, "Villa")] <- "Villa Dhuveli"
VesselLog$Vname[str_detect(VesselLog$Vname, "Dhuve")] <- "Villa Dhuveli"
VesselLog$Vname[str_detect(VesselLog$Vname, "Miri")] <- "Mirihi"
VesselLog$Vname[str_detect(VesselLog$Vname, "Eagle")] <- "Eagle Ray"
VesselLog$Vname[str_detect(VesselLog$Vname, "DIVE DHONI FOR EAGLE RAY")] <- "Eagle Ray"
VesselLog$Vname[str_detect(VesselLog$Vname, "Plan")] <- "Plan Hotel"
VesselLog$Vname[str_detect(VesselLog$Vname, "omaf")] <- "Gomafulhu"
VesselLog$Vname[str_detect(VesselLog$Vname, "afari")] <- "Safari"
VesselLog$Vname[str_detect(VesselLog$Vname, "Lux")] <- "Lux"
VesselLog$Vname[str_detect(VesselLog$Vname, "LUX")] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Dehayaa" ] <- "Dheriyaa"
VesselLog$Vname[VesselLog$Vname == "Dheriya" ] <- "Dheriyaa"
VesselLog$Vname[VesselLog$Vname == "Dheryaa" ] <- "Dheriyaa"
VesselLog$Vname[VesselLog$Vname == "Dhiraha" ] <- "Dheriyaa"
VesselLog$Vname[VesselLog$Vname == "Dhoni  yala" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "Dhoni YALA" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "Dhoni yala" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "Dohni yala" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "Dhoniyala" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "Dhuvili" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "Dhiveli" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "Divili" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "Dhuvili 6" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "Divli" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "Dive  " ] <- "Dive Maldives"
VesselLog$Vname[VesselLog$Vname == "Dive Maldives" ] <- "Dive Maldives"
VesselLog$Vname[VesselLog$Vname == "DiveDhoni" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Dream Catcher " ] <- "Dream Catcher"
VesselLog$Vname[VesselLog$Vname == "Dream Yacht (Dream Maldives)" ] <- "Dream Catcher"
VesselLog$Vname[VesselLog$Vname == "Diva Theia" ] <- "Dive Theia"
VesselLog$Vname[VesselLog$Vname == "EPISODE" ] <- "Episode"
VesselLog$Vname[VesselLog$Vname == "ESCAPE" ] <- "Escape"
VesselLog$Vname[VesselLog$Vname == "Escape W Maldives" ] <- "Escape"
VesselLog$Vname[VesselLog$Vname == "Euro Divers Maldives" ] <- "Euro"
VesselLog$Vname[VesselLog$Vname == "Explorer Carpe Vita" ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Explorer's Tender" ] <- "Explorer"
VesselLog$Vname[VesselLog$Vname == "Etheergeriya" ] <- "Etheekiyva"
VesselLog$Vname[VesselLog$Vname == "Enjoy real hospitality" ] <- "Plan Hotel"
VesselLog$Vname[VesselLog$Vname == "FAZA" ] <- "Faza"
VesselLog$Vname[VesselLog$Vname == "Green Maharhi" ] <- "Green"
VesselLog$Vname[VesselLog$Vname == "Gurahalil" ] <- "Gurahari"
VesselLog$Vname[VesselLog$Vname == "Halema" ] <- "Haleema"
VesselLog$Vname[VesselLog$Vname == "Haliviews" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "Halivumas" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "Halucimas" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "HHALUVIMAS" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "Haluvimus" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "Hermione" ] <- "Hermine"
VesselLog$Vname[VesselLog$Vname == "Holiady" ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Holiday" ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Holiday Cruise" ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Holiday island" ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Horizon III" ] <- "Horizon"
VesselLog$Vname[VesselLog$Vname == "Hotel Plan" ] <- "Plan Hotel"
VesselLog$Vname[VesselLog$Vname == "Empress Muni" ] <- "Impress Muni"
VesselLog$Vname[VesselLog$Vname == "Impress Muni  " ] <- "Impress Muni"
VesselLog$Vname[VesselLog$Vname == "Impressmuni" ] <- "Impress Muni"
VesselLog$Vname[VesselLog$Vname == "Island Safari 1" ] <- "Island Safari"
VesselLog$Vname[VesselLog$Vname == "Island Safari 2" ] <- "Island Safari"
VesselLog$Vname[VesselLog$Vname == "Island Safari 3" ] <- "Island Safari"
VesselLog$Vname[VesselLog$Vname == "Jihaad" ] <- "Jihaadh"
VesselLog$Vname[VesselLog$Vname == "Jihad" ] <- "Jihaadh"
VesselLog$Vname[VesselLog$Vname == "Jihead" ] <- "Jihaadh"
VesselLog$Vname[VesselLog$Vname == "Kaamiaab" ] <- "Kaamiyaab"
VesselLog$Vname[VesselLog$Vname == "Kafe" ] <- "Kafi"
VesselLog$Vname[VesselLog$Vname == "KAFI" ] <- "Kafi"
VesselLog$Vname[VesselLog$Vname == "Kofi" ] <- "Kafi"
VesselLog$Vname[VesselLog$Vname == "Koimalia" ] <- "Koimala"
VesselLog$Vname[VesselLog$Vname == "Koca Alaa" ] <- "Kokala"
VesselLog$Vname[VesselLog$Vname == "KOKALA" ] <- "Kokala"
VesselLog$Vname[VesselLog$Vname == "Kokaaha" ] <- "Kokala"
VesselLog$Vname[VesselLog$Vname == "Koka aza" ] <- "Kokala"
VesselLog$Vname[VesselLog$Vname == "LAAL" ] <- "Laal"
VesselLog$Vname[VesselLog$Vname == "Liali Beach" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Lila Beach" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Lilly" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Lilly Beach" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Lily" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Lily " ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Loo Loo 3" ] <- "Loo Loo"
VesselLog$Vname[VesselLog$Vname == "Looloo" ] <- "Loo Loo"
VesselLog$Vname[VesselLog$Vname == "Lou Lou 3" ] <- "Loo Loo"
VesselLog$Vname[VesselLog$Vname == "Lulu" ] <- "Loo Loo"
VesselLog$Vname[VesselLog$Vname == "LUX" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "AMAAZ" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Amaaz" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Amaaz 2" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Lux - AMAAZ-2" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Luxe" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "LUXE" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Luxe Dive Dhoni" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "LVBDD" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "LLVBDD Tender" ] <- "Tender"
VesselLog$Vname[VesselLog$Vname == "Maafushi Varu" ] <- "Maafushivaru"
VesselLog$Vname[VesselLog$Vname == "Mafusi" ] <- "Maafushivaru"
VesselLog$Vname[VesselLog$Vname == "Maharhi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Maldivan Dream" ] <- "Maldivian Dream"
VesselLog$Vname[VesselLog$Vname == "MALDIVIAN DREAM" ] <- "Maldivian Dream"
VesselLog$Vname[VesselLog$Vname == "Maldiva Star" ] <- "Maldivian Stars"
VesselLog$Vname[VesselLog$Vname == "Maldives Boat Club" ] <- "Maldives Boatclub.com"
VesselLog$Vname[VesselLog$Vname == "Maldivesboatclub.com" ] <- "Maldives Boatclub.com"
VesselLog$Vname[VesselLog$Vname == "Maldivian stars" ] <- "Maldivian Star"
VesselLog$Vname[VesselLog$Vname == "Maldivian Starts" ] <- "Maldivian Star"
VesselLog$Vname[VesselLog$Vname == "Maldivian Sun" ] <- "Maldivian Star"
VesselLog$Vname[VesselLog$Vname == "Maldiviana Fender" ] <- "Maldiviana"
VesselLog$Vname[VesselLog$Vname == "Maldiviano" ] <- "Maldiviana"
VesselLog$Vname[VesselLog$Vname == "Mamajan" ] <- "Mama Jana"
VesselLog$Vname[VesselLog$Vname == "Manta 2" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Manta II" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Manta2" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Manthri" ] <- "Manthiri"
VesselLog$Vname[VesselLog$Vname == "Mariana" ] <- "Marina"
VesselLog$Vname[VesselLog$Vname == "Marine" ] <- "Marina"
VesselLog$Vname[VesselLog$Vname == "Marine 1" ] <- "Marina"
VesselLog$Vname[VesselLog$Vname == "Marlin" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Marlin Lilly Beach" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Merihi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mihiri Thari" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Minhi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirahi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirahi Thari" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Minhi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mira" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirahi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirahi Thari" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirhi Dhoni" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "MIRIHI" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirihi  " ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirihi Marine" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirihi Sailboat" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirihi Thari" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "MIRIHI THARI" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirili" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirini Dhoni" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirith" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mofufushi" ] <- "Moofushi"
VesselLog$Vname[VesselLog$Vname == "Moofushi 1" ] <- "Moofushi"
VesselLog$Vname[VesselLog$Vname == "Moofushi 7" ] <- "Moofushi"
VesselLog$Vname[VesselLog$Vname == "Moofushi I" ] <- "Moofushi"
VesselLog$Vname[VesselLog$Vname == "Moonima" ] <- "Moonimaa"
VesselLog$Vname[VesselLog$Vname == "Ms Sea Spirit" ] <- "MV Seaspirit"
VesselLog$Vname[VesselLog$Vname == "My Sea Spirit" ] <- "MV Seaspirit"
VesselLog$Vname[VesselLog$Vname == "My Maldivian Princess" ] <- "MV Maldives Princess"
VesselLog$Vname[VesselLog$Vname == "My Sea Queen" ] <- "MV Sea Queen"
VesselLog$Vname[VesselLog$Vname == "My Sea Queen Luna DD" ] <- "MV Sea Queen"
VesselLog$Vname[VesselLog$Vname == "NA Mona (Lux)" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Naahy" ] <- "Naazy"
VesselLog$Vname[VesselLog$Vname == "NAAZY" ] <- "Naazy"
VesselLog$Vname[VesselLog$Vname == "Naazy (blue force)" ] <- "Naazy"
VesselLog$Vname[VesselLog$Vname == "Namgona" ] <- "Namoona"
VesselLog$Vname[VesselLog$Vname == "NASEEB" ] <- "Naseeb"
VesselLog$Vname[VesselLog$Vname == "Nasra" ] <- "Nasru"
VesselLog$Vname[VesselLog$Vname == "Nautilus 1" ] <- "Nautilus"
VesselLog$Vname[VesselLog$Vname == "Nautilus One" ] <- "Nautilus"
VesselLog$Vname[VesselLog$Vname == "Nohiri" ] <- "Noohiri"
VesselLog$Vname[VesselLog$Vname == "Noohiri 2" ] <- "Noohiri"
VesselLog$Vname[VesselLog$Vname == "Noohisi" ] <- "Noohiri"
VesselLog$Vname[VesselLog$Vname == "Noovilla" ] <- "Noohiri"
VesselLog$Vname[VesselLog$Vname == "Ocean Pro Mac" ] <- "Ocean Pro"
VesselLog$Vname[VesselLog$Vname == "Ocean Saphire" ] <- "NOcean Sapphire"
VesselLog$Vname[VesselLog$Vname == "ORCA" ] <- "Orca"
VesselLog$Vname[VesselLog$Vname == "ORION" ] <- "MV Orion"
VesselLog$Vname[VesselLog$Vname == "P26 86A 0107-1" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "P26689109-1" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "P2771A-7-4" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "P2954A-04-06-1" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "P3754A/12" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Plan b hotel" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Plan B Hotel" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Plan Hotel" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Plan Hotel Touring 36" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Plan Hotels" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "PRINCESS DHONHAMINA" ] <- "Princess Dhon Kamana"
VesselLog$Vname[VesselLog$Vname == "Princess Dhonkamaa" ] <- "Princess Dhon Kamana"
VesselLog$Vname[VesselLog$Vname == "Princess Lara Dive Dhoni" ] <- "Princess Lara"
VesselLog$Vname[VesselLog$Vname == "PZ174-02" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "PZ884B" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Raace" ] <- "Raaee"
VesselLog$Vname[VesselLog$Vname == "RAAEE" ] <- "Raaee"
VesselLog$Vname[VesselLog$Vname == "Race 2" ] <- "Raaee"
VesselLog$Vname[VesselLog$Vname == "Ranaaee" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Rangali big game" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Rangali Raani" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Rangali Rani" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Range" ] <- "Ranj"
VesselLog$Vname[VesselLog$Vname == "RANJ" ] <- "Ranj"
VesselLog$Vname[VesselLog$Vname == "Ranni" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Reed Watch 3" ] <- "Reef Watch"
VesselLog$Vname[VesselLog$Vname == "Reef Watch 3" ] <- "Reef Watch"
VesselLog$Vname[VesselLog$Vname == "Reef Water" ] <- "Reef Watch"
VesselLog$Vname[VesselLog$Vname == "Rehedi" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Rehendhi" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Rhandi" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Roharti (Conrad)" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Rohendi" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Rasalachi" ] <- "Sachika Dive"
VesselLog$Vname[VesselLog$Vname == "Sachika" ] <- "Sachika Dive"
VesselLog$Vname[VesselLog$Vname == "Sachiku Dive" ] <- "Sachika Dive"
VesselLog$Vname[VesselLog$Vname == "Safari 2" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "Safari 3" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "safari 5" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "Safari 5" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "Safari1" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "Safari3" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "Sata" ] <- "Sataa"
VesselLog$Vname[VesselLog$Vname == "Safari Maldives" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "Sechie Dive" ] <- "Sachika Dive"
VesselLog$Vname[VesselLog$Vname == "Seefari" ] <- "Safari"
VesselLog$Vname[VesselLog$Vname == "Shadha" ] <- "Sdhadhee"
VesselLog$Vname[VesselLog$Vname == "Shiloh" ] <- "Solei"
VesselLog$Vname[VesselLog$Vname == "Siriana" ] <- "Sirina"
VesselLog$Vname[VesselLog$Vname == "Solail" ] <- "Solei"
VesselLog$Vname[VesselLog$Vname == "Solei" ] <- "Solei"
VesselLog$Vname[VesselLog$Vname == "SOLEIC" ] <- "Solei"
VesselLog$Vname[VesselLog$Vname == "Soleil" ] <- "Solei"
VesselLog$Vname[VesselLog$Vname == "Soleil DD" ] <- "Solei"
VesselLog$Vname[VesselLog$Vname == "Sting Raz" ] <- "Sting Ray"
VesselLog$Vname[VesselLog$Vname == "Sun island" ] <- "Sun Island"
VesselLog$Vname[VesselLog$Vname == "Sunset 2" ] <- "Sunset Queen"
VesselLog$Vname[VesselLog$Vname == "Sunset 10" ] <- "Sunset Queen"
VesselLog$Vname[VesselLog$Vname == "Sunrise Queen" ] <- "Sunset Queen"
VesselLog$Vname[VesselLog$Vname == "Sun island" ] <- "Sun Island"
VesselLog$Vname[VesselLog$Vname == "Terra (Sun Island)" ] <- "Sun Island"
VesselLog$Vname[VesselLog$Vname == "Theia Dive Dhoni" ] <- "Their"
VesselLog$Vname[VesselLog$Vname == "Tornado Anyaya" ] <- "Tornado"
VesselLog$Vname[VesselLog$Vname == "Touring" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Touring 36" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Tula Fushi" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Touring" ] <- "Thulafushi"
VesselLog$Vname[VesselLog$Vname == "Whale shark" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "Whale Shark 2" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "whaleshark" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "White (?) " ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "White Shark" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "whale shark" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "White Turn" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "World light" ] <- "World Light"
VesselLog$Vname[VesselLog$Vname == "Zanzil" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Zeino" ] <- "Zenio"
VesselLog$Vname[VesselLog$Vname == "Zemio" ] <- "Zenio"
VesselLog$Vname[VesselLog$Vname == "Zemo" ] <- "Zenio"
VesselLog$Vname[VesselLog$Vname == "ZENIO" ] <- "Zenio"
VesselLog$Vname[VesselLog$Vname == "Zeno" ] <- "Zenio"
VesselLog$Vname[VesselLog$Vname == "ZIRCON" ] <- "Zircon"
VesselLog$Vname[VesselLog$Vname == "Zirkon" ] <- "Zircon"
VesselLog$Vname[VesselLog$Vname == "Ziron" ] <- "Zircon"
VesselLog$Vname[VesselLog$Vname == "Ari Royal" ] <- "Ark Royal"
VesselLog$Vname[VesselLog$Vname == "AS Marine" ] <- "Marine"
VesselLog$Vname[VesselLog$Vname == "Blue Force Wazy" ] <- "Naazy"
VesselLog$Vname[VesselLog$Vname == "Blue mare" ] <- "Blue Marine"
VesselLog$Vname[VesselLog$Vname == "Impress Muni" ] <- "Impress Muni"
VesselLog$Vname[VesselLog$Vname == "Impressumi" ] <- "Impress Muni"
VesselLog$Vname[VesselLog$Vname == "Isaf" ] <- "Isan"
VesselLog$Vname[VesselLog$Vname == "ISAN" ] <- "Isan"
VesselLog$Vname[VesselLog$Vname == "Isun" ] <- "Isan"
VesselLog$Vname[VesselLog$Vname == "Jiheah" ] <- "Jihaadh"
VesselLog$Vname[VesselLog$Vname == "Maldivian Stars" ] <- "Maldivian Star"
VesselLog$Vname[VesselLog$Vname == "moofushi I" ] <- "Moofushi"
VesselLog$Vname[VesselLog$Vname == "Sacheira" ] <- "Sachika Dive"
VesselLog$Vname[VesselLog$Vname == "Sechie  Dive " ] <- "Isan"
VesselLog$Vname[VesselLog$Vname == "soleil" ] <- "Solei"
VesselLog$Vname[VesselLog$Vname == "villa dhuveli 58" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "VILLA HOTEL60 DVEHNI" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "V.D.60" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Ulla Dhauveli 39" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "Vaharufalhi" ] <- "Vakarufali"
VesselLog$Vname[VesselLog$Vname == "Vakarah Valley" ] <- "Vakarufali"
VesselLog$Vname[VesselLog$Vname == "Vakarnfali" ] <- "Vakarufali"
VesselLog$Vname[VesselLog$Vname == "Vakrufahli" ] <- "Vakarufali"
VesselLog$Vname[VesselLog$Vname == "Varburfuli" ] <- "Vakarufali"
VesselLog$Vname[VesselLog$Vname == "whale shark" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "Whaleshark" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "Wolrd light" ] <- "World Light"
VesselLog$Vname[VesselLog$Vname == "Zanil" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Impress Muni " ] <- "Impress Muni"
VesselLog$Vname[VesselLog$Vname == "Luc" ] <- "Lux"
VesselLog$Vname[VesselLog$Vname == "Coupe Vita" ] <- "Carpe Vita"
VesselLog$Vname[VesselLog$Vname == "Eagle Ray ?" ] <- "Eagle Ray"
VesselLog$Vname[VesselLog$Vname == "Gomafhlu" ] <- "Gomafulhu"
VesselLog$Vname[VesselLog$Vname == "Gomafula" ] <- "Gomafulhu"
VesselLog$Vname[VesselLog$Vname == "Gomafolhu" ] <- "Gomafulhu"
VesselLog$Vname[VesselLog$Vname == "gomafulhu" ] <- "Gomafulhu"
VesselLog$Vname[VesselLog$Vname == "DIVE DHONI FOR EAGLE RAY" ] <- "Eagle Ray"
VesselLog$Vname[VesselLog$Vname == "Eagle Ray ?" ] <- "Eagle Ray"
VesselLog$Vname[VesselLog$Vname == "Haliviwes" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "HALUVIMAS" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "Island Safari " ] <- "Island Safari"
VesselLog$Vname[VesselLog$Vname == "Mirihi  " ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Maauali" ] <- "Maaralhi"
VesselLog$Vname[VesselLog$Vname == "Maldivian Dream " ] <- "Maldivian Dream"
VesselLog$Vname[VesselLog$Vname == "HV Seagreen" ] <- "MV Sea Queen"
VesselLog$Vname[VesselLog$Vname == "Kembala" ] <- "Koimala"
VesselLog$Vname[VesselLog$Vname == "Blue Shark Two" ] <- "Blue Shark"
VesselLog$Vname[VesselLog$Vname == "Blue Wave Touring 36" ] <- "Blue Wave"
VesselLog$Vname[VesselLog$Vname == "Blueshark 2" ] <- "Blue Shark"
VesselLog$Vname[VesselLog$Vname == "C77188" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Cape Vita" ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Carpe Vita" ] <- "Carpe Vitta"
VesselLog$Vname[VesselLog$Vname == "Carpe Diem " ] <- "Carpe Diem"
VesselLog$Vname[VesselLog$Vname == "Carpe diem?" ] <- "Carpe Diem"
VesselLog$Vname[VesselLog$Vname == "Centauro" ] <- "Centara"
VesselLog$Vname[VesselLog$Vname == "Conrad Diving" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Conrad Speed Boat" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "CONSTANCE" ] <- "Cnstance"
VesselLog$Vname[VesselLog$Vname == "Consupa" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "D ?" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Caainkan' Baa" ] <- "Dhainkan baa"
VesselLog$Vname[VesselLog$Vname == "Dhoni Gala" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "CDhJow Magili Stella" ] <- "Dhonimigili"
VesselLog$Vname[VesselLog$Vname == "Dolphin " ] <- "Dolphin"
VesselLog$Vname[VesselLog$Vname == "DHUVELI 9" ] <- "Villa Dhuveli"
VesselLog$Vname[VesselLog$Vname == "Dhonivala" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Haluvima" ] <- "Halivimas"
VesselLog$Vname[VesselLog$Vname == "Hammerhead 2" ] <- "Hammerhead"
VesselLog$Vname[VesselLog$Vname == " Holdiay Island" ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Holiday " ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Horizon 3" ] <- "Horizon"
VesselLog$Vname[VesselLog$Vname == "Impress Munie" ] <- "Impress Muni"
VesselLog$Vname[VesselLog$Vname == "Isis cruiser" ] <- "Isis Cruiser"
VesselLog$Vname[VesselLog$Vname == "Jihaadi" ] <- "Jihaadh"
VesselLog$Vname[VesselLog$Vname == "Jinaadh" ] <- "Jihaadh"
VesselLog$Vname[VesselLog$Vname == "Kaami Vaab" ] <- "Kaamiyaab"
VesselLog$Vname[VesselLog$Vname == "Kaamyaab" ] <- "Kaamiyaab"
VesselLog$Vname[VesselLog$Vname == "KETHI" ] <- "Kethi"
VesselLog$Vname[VesselLog$Vname == "Lou lou 3" ] <- "Loo Loo"
VesselLog$Vname[VesselLog$Vname == "Maldivan Dream" ] <- "Maldivian Dream"
VesselLog$Vname[VesselLog$Vname == "Maldivian Agressor" ] <- "Maldives Aggressor"
VesselLog$Vname[VesselLog$Vname == "maldiviana" ] <- "Maldiviana"
VesselLog$Vname[VesselLog$Vname == "MALDIVIANA" ] <- "Maldiviana"
VesselLog$Vname[VesselLog$Vname == "Maledivesboatclub.com" ] <- "Maldives Boatclub.com"
VesselLog$Vname[VesselLog$Vname == "Marine" ] <- "Marina"
VesselLog$Vname[VesselLog$Vname == "MIHIRI " ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Mirrihi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == "Moldivana" ] <- "Maldiviana"
VesselLog$Vname[VesselLog$Vname == "Moldivia Dream" ] <- "Maldivian Dream"
VesselLog$Vname[VesselLog$Vname == "MV Carpe Diem" ] <- "Carpe Diem"
VesselLog$Vname[VesselLog$Vname == "Naazi" ] <- "Naazy"
VesselLog$Vname[VesselLog$Vname == "Naazey" ] <- "Naazy"
VesselLog$Vname[VesselLog$Vname == "Namoonu" ] <- "Namoona"
VesselLog$Vname[VesselLog$Vname == "Naoona" ] <- "Namoona"
VesselLog$Vname[VesselLog$Vname == "Nobula" ] <- "Mobula"
VesselLog$Vname[VesselLog$Vname == "NOcean Sapphire" ] <- "Ocean Sapphire"
VesselLog$Vname[VesselLog$Vname == "Nomooma" ] <- "Namoona"
VesselLog$Vname[VesselLog$Vname == "P?" ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == "Princess Dhonkamana" ] <- "Princess Dhon Kamana"
VesselLog$Vname[VesselLog$Vname == "PRINCESS LARA" ] <- "Princess Lara"
VesselLog$Vname[VesselLog$Vname == "Princess Ushuc" ] <- "Princess Ushwa"
VesselLog$Vname[VesselLog$Vname == "Ra?ee" ] <- "Raaee"
VesselLog$Vname[VesselLog$Vname == "Reef Watch" ] <- "Maafushivaru"
VesselLog$Vname[VesselLog$Vname == "Reef watch 2" ] <- "Maafushivaru"
VesselLog$Vname[VesselLog$Vname == "Sachika dive" ] <- "Sachika Dive"
VesselLog$Vname[VesselLog$Vname == "SEA QUEEN" ] <- "MV Sea Queen"
VesselLog$Vname[VesselLog$Vname == "Sea Spirit" ] <- "MV Sea Spirit"
VesselLog$Vname[VesselLog$Vname == "MV Seaspirit" ] <- "MV Sea Spirit"
VesselLog$Vname[VesselLog$Vname == "Shanifa" ] <- "Sharifa"
VesselLog$Vname[VesselLog$Vname == "SHARIFA" ] <- "Sharifa"
VesselLog$Vname[VesselLog$Vname == "SOUTHERN CROSS" ] <- "Southern Cross"
VesselLog$Vname[VesselLog$Vname == "STING RAY" ] <- "Sting Ray"
VesselLog$Vname[VesselLog$Vname == "SUN ISLAND" ] <- "Sun Island"
VesselLog$Vname[VesselLog$Vname == "Sun island " ] <- "Sun Island"
VesselLog$Vname[VesselLog$Vname == "Tender Carpe Diem" ] <- "Carpe Diem"
VesselLog$Vname[VesselLog$Vname == "Tonado" ] <- "Tornado"
VesselLog$Vname[VesselLog$Vname == "Tritent" ] <- "Triton"
VesselLog$Vname[VesselLog$Vname == "TRITON" ] <- "Triton"
VesselLog$Vname[VesselLog$Vname == "TRITON " ] <- "Triton"
VesselLog$Vname[VesselLog$Vname == "USAA" ] <- "Usaa"
VesselLog$Vname[VesselLog$Vname == "Vakarufali" ] <- "Vakurahfahli"
VesselLog$Vname[VesselLog$Vname == "Whaleshark 2" ] <- "Whale Shark"
VesselLog$Vname[VesselLog$Vname == "zircon" ] <- "Zircon"
VesselLog$Vname[VesselLog$Vname == "." ] <- "Unknown"
VesselLog$Vname[VesselLog$Vname == " Blue Shark" ] <- "Blue Shark"
VesselLog$Vname[VesselLog$Vname == "Aramagu" ] <- "Araamagu"
VesselLog$Vname[VesselLog$Vname == "ARI QUEEN" ] <- "Ari Queen"
VesselLog$Vname[VesselLog$Vname == "Ari Venture" ] <- "Ark Venture"
VesselLog$Vname[VesselLog$Vname == "Atoll Chelenger" ] <- "Atoll Challenger"
VesselLog$Vname[VesselLog$Vname == "Canopus" ] <- "CanopusMaldives.com"
VesselLog$Vname[VesselLog$Vname == "Cnstance" ] <- "Constance"
VesselLog$Vname[VesselLog$Vname == "Condad Diving" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "conrad" ] <- "Conrad"
VesselLog$Vname[VesselLog$Vname == "Dhigureh Guest House" ] <- "Island Divers"
VesselLog$Vname[VesselLog$Vname == "Dhonihiya" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "Etheekiyva" ] <- "Eheetheriya"
VesselLog$Vname[VesselLog$Vname == "GOFI" ] <- "Gofi"
VesselLog$Vname[VesselLog$Vname == "Halivimas" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "Hallivumas" ] <- "Haluvimas"
VesselLog$Vname[VesselLog$Vname == "Hirihi" ] <- "Mirihi"
VesselLog$Vname[VesselLog$Vname == " Holdiay Island" ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Island Divers Digurah" ] <- "Island Divers"
VesselLog$Vname[VesselLog$Vname == "Lili Beach" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "Lili beach" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "MALDIVIAN STAR" ] <- "Maldivian Star"
VesselLog$Vname[VesselLog$Vname == "MV See Spirit" ] <- "MV Sea Spirit"
VesselLog$Vname[VesselLog$Vname == "Nortulous" ] <- "Nautilus"
VesselLog$Vname[VesselLog$Vname == "One + Only" ] <- "One & Only"
VesselLog$Vname[VesselLog$Vname == "Priton" ] <- "Triton"

# Seccond round of shitty corrections
VesselLog$Vname[VesselLog$Vname == "Dhiriyaa" ] <- "Dheriyaa"
VesselLog$Vname[VesselLog$Vname == "Dhonhiya" ] <- "Dhoni Yala"
VesselLog$Vname[VesselLog$Vname == "Dive " ] <- "Dive"
VesselLog$Vname[VesselLog$Vname == "Don Komona" ] <- "Don Konomara"
VesselLog$Vname[VesselLog$Vname == "Euro Divers" ] <- "Euro"
VesselLog$Vname[VesselLog$Vname == "Halima" ] <- "Haleema"
VesselLog$Vname[VesselLog$Vname == "Hammerhead Attamas" ] <- "Hammerhead"
VesselLog$Vname[VesselLog$Vname == "Holdiay Island" ] <- "Holiday Island"
VesselLog$Vname[VesselLog$Vname == "Island Divers Dighurah" ] <- "Island Divers"
VesselLog$Vname[VesselLog$Vname == "Lily beach" ] <- "Lily Beach"
VesselLog$Vname[VesselLog$Vname == "LVBDD of" ] <- "Name Unseen"
VesselLog$Vname[VesselLog$Vname == "LVBDD Tender" ] <- "Name Unseen"
VesselLog$Vname[VesselLog$Vname == "Maavani" ] <- "Maavahi"
VesselLog$Vname[VesselLog$Vname == "Maldivan Dream " ] <- "Maldivian Dream"
VesselLog$Vname[VesselLog$Vname == "Maledives.diving.com" ] <- "Maldives Diving"
VesselLog$Vname[VesselLog$Vname == "Maldives Boat Club Stingray" ] <- "Maldives Boatclub.com"
VesselLog$Vname[VesselLog$Vname == "Menna" ] <- "Meena"
VesselLog$Vname[VesselLog$Vname == "Mobular" ] <- "Mobula"
VesselLog$Vname[VesselLog$Vname == "Moonimar" ] <- "Moonimaa"
VesselLog$Vname[VesselLog$Vname == "Mujaa" ] <- "Munajaa"
VesselLog$Vname[VesselLog$Vname == "Name Unseen" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "N/A" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "Nauris" ] <- "Nauras"
VesselLog$Vname[VesselLog$Vname == "Nimath" ] <- "Niumath"
VesselLog$Vname[VesselLog$Vname == "NN" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "Pole and Line Fishing Boat" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "Princess 42" ] <- "Princess"
VesselLog$Vname[VesselLog$Vname == "Randalai " ] <- "Randai"
VesselLog$Vname[VesselLog$Vname == "PIVON?" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "Reef Watch " ] <- "Reef Watch"
VesselLog$Vname[VesselLog$Vname == "Raan" ] <- "Rana"
VesselLog$Vname[VesselLog$Vname == "Rnnaa" ] <- "Rana"
VesselLog$Vname[VesselLog$Vname == "santara" ] <- "Centara"
VesselLog$Vname[VesselLog$Vname == "Saveya" ] <- "Saveyra"
VesselLog$Vname[VesselLog$Vname == "Savrya" ] <- "Saveyra"
VesselLog$Vname[VesselLog$Vname == "Sea   Dream" ] <- "Sea Dream"
VesselLog$Vname[VesselLog$Vname == "Seadream" ] <- "Sea Dream"
VesselLog$Vname[VesselLog$Vname == "Sdhadhee" ] <- "Shadhee"
VesselLog$Vname[VesselLog$Vname == "Shadihaa" ] <- "Shadhee"
VesselLog$Vname[VesselLog$Vname == "Shaadaas" ] <- "Shadaa's"
VesselLog$Vname[VesselLog$Vname == "Shandhads" ] <- "Shadaa's"
VesselLog$Vname[VesselLog$Vname == "Shandaa" ] <- "Shadaa's"
VesselLog$Vname[VesselLog$Vname == "Speed boat for larger boat" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "Stella 1" ] <- "Stella"
VesselLog$Vname[VesselLog$Vname == "Stella 2" ] <- "Stella"
VesselLog$Vname[VesselLog$Vname == "Target' - Local Fishing Boat" ] <- "Target"
VesselLog$Vname[VesselLog$Vname == "Tender" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "Unknown" ] <- "NA"
VesselLog$Vname[VesselLog$Vname == "Usha" ] <- "Usaa"
VesselLog$Vname[VesselLog$Vname == "Vakuruafalhi" ] <- "Vakurahfahli"
VesselLog$Vname[VesselLog$Vname == "WHALESHARK2" ] <- "Whale Shark"

VesselLog$Vname <- as.factor (VesselLog$Vname)
VesselLog$Vname <- droplevels (VesselLog$Vname)
table(VesselLog$Vname)

# Generate list of individual vessels 
VesselLog$NameType <- paste (VesselLog$Vname, VesselLog$Type)

# Take only one year of data from 2012 - 2013 (Nov)
# VesselLog <- VesselLog[VesselLog$DateTime > as.POSIXct("2012-11-01") & VesselLog$DateTime < as.POSIXct("2013-11-01"), ]
Vessels <- as.data.frame.table(tapply(VesselLog$UnitPrice, VesselLog$NameType, max, na.rm=TRUE))
names(Vessels) <- c ("NameType", "PricePP")
Vessels$PricePP[Vessels$PricePP == -Inf] <- NA


Vessels$PricePP[Vessels$NameType == "CanopusMaldives.com LVBDD" ] <- 125
Vessels$PricePP[Vessels$NameType == "CanopusMaldives.com RDD" ] <- 125
Vessels$PricePP[Vessels$NameType == "Conrad RSFB" ] <- 667
Vessels$PricePP[Vessels$NameType == "Conrad RSB" ] <- 200
# CHanged price for Dream Catcher from 186 to 285
Vessels$PricePP[Vessels$NameType == "Dream Catcher LVBDD" ] <- 285
Vessels$PricePP[Vessels$NameType == "Dream Catcher LVB" ] <- 285
Vessels$PricePP[Vessels$NameType == "Dive Maldives LVBDD" ] <- 341
Vessels$PricePP[Vessels$NameType == "Dive Maldives RDD" ] <- 341
Vessels$PricePP[Vessels$NameType == "Dolphin RDD" ] <- 118
Vessels$PricePP[Vessels$NameType == "Faza RDD" ] <- 118
Vessels$PricePP[Vessels$NameType == "Maldivian Dream " ] <- 273
Vessels$PricePP[Vessels$NameType == "Mirihi LVB" ] <- 141
Vessels$PricePP[Vessels$NameType == "Princess Haleema LVBDD" ] <- 250
Vessels$PricePP[Vessels$NameType == "Sachika Dive RED" ] <- 304
Vessels$PricePP[Vessels$NameType == "Star Divers LVBDD" ] <- 132
Vessels$PricePP[Vessels$NameType == "Theia Tender" ] <- 259
Vessels$PricePP[Vessels$NameType == "Thulafushi RDD" ] <- 150
Vessels$PricePP[Vessels$NameType == "Thulafushi RED" ] <- 150
Vessels$PricePP[Vessels$NameType == "Habiyli Azul Maldives LVB" ] <- 211
Vessels$PricePP[Vessels$NameType == "Honveli Azul LVB" ] <- 211
Vessels$PricePP[Vessels$NameType == "Kafi LVB" ] <- 154
Vessels$PricePP[Vessels$NameType == "Kefi LVB" ] <- 154
Vessels$PricePP[Vessels$NameType == "Kefi LVBDD" ] <- 154
Vessels$PricePP[Vessels$NameType == "Koimala LVB" ] <- 256
Vessels$PricePP[Vessels$NameType == "Maldivan Dream  LVB" ] <- 312
Vessels$PricePP[Vessels$NameType == "Maldives Boatclub.com LVBDD" ] <- 211
Vessels$PricePP[Vessels$NameType == "Maldives Boatclub.com RDD" ] <- 211
Vessels$PricePP[Vessels$NameType == "Manthiri LVB" ] <- 130
Vessels$PricePP[Vessels$NameType == "Princess Dhon Kamana LVB" ] <- 197
Vessels$PricePP[Vessels$NameType == "Dhoni Yala RSD" ] <- 200
# Added later
Vessels$PricePP[Vessels$NameType == "MV Orion LVB" ] <- 370
Vessels$PricePP[Vessels$NameType == "MV Orion LVBDD" ] <- 370
Vessels$PricePP[Vessels$NameType == "Leo LVB" ] <- 275
Vessels$PricePP[Vessels$NameType == "Leo RDD" ] <- 275
Vessels$PricePP[Vessels$NameType == "Carpe Vitta LVB" ] <- 395
Vessels$PricePP[Vessels$NameType == "Carpe Vitta LVBDD" ] <- 395
Vessels$PricePP[Vessels$NameType == "Carpe Diem LVBDD" ] <- 284
Vessels$PricePP[Vessels$NameType == "Carpe Diem LVB" ] <- 284
Vessels$PricePP[Vessels$NameType == "Carpe Diem Tender" ] <- 284
Vessels$PricePP[Vessels$NameType == "Aurora LVB" ] <- 314
Vessels$PricePP[Vessels$NameType == "Theia LVBDD" ] <- 336
Vessels$PricePP[Vessels$NameType == "Theia Diem LVB" ] <- 336
Vessels$PricePP[Vessels$NameType == "Theia Diem Tender" ] <- 336
Vessels$PricePP[Vessels$NameType == "Princess Ushwa LVBDD" ] <- 270
Vessels$PricePP[Vessels$NameType == "Princess Ushwa LVB" ] <- 270
Vessels$PricePP[Vessels$NameType == "Ari Queen Tender" ] <- 275
Vessels$PricePP[Vessels$NameType == "Ari Queen LVBDD" ] <- 275
Vessels$PricePP[Vessels$NameType == "Ari Queen LVB" ] <- 275
Vessels$PricePP[Vessels$NameType == "Adventure 2 LVB" ] <- 341
Vessels$PricePP[Vessels$NameType == "Sachika Dive LVB" ] <- 297
Vessels$PricePP[Vessels$NameType == "Sachika Dive LVBDD" ] <- 297
Vessels$PricePP[Vessels$NameType == "Eagle Ray LVBDD" ] <- 279
Vessels$PricePP[Vessels$NameType == "Eagle Ray LVB" ] <- 279
Vessels$PricePP[Vessels$NameType == "Eagle Ray RDD" ] <- 279
Vessels$PricePP[Vessels$NameType == "Eagle Ray RED" ] <- 279
Vessels$PricePP[Vessels$NameType == "Sharifa LVB" ] <- 330
Vessels$PricePP[Vessels$NameType == "Sharifa Tender" ] <- 330
Vessels$PricePP[Vessels$NameType == "Blue Lagoon LVB" ] <- 280
Vessels$PricePP[Vessels$NameType == "Sting Ray LVB" ] <- 215
Vessels$PricePP[Vessels$NameType == "Sting Ray LVBDD" ] <- 215
Vessels$PricePP[Vessels$NameType == "Maavahi LVB" ] <- 240
Vessels$PricePP[Vessels$NameType == "Ark Royal LVB" ] <- 320
Vessels$PricePP[Vessels$NameType == "Ark Royal LVBDD" ] <- 320
Vessels$PricePP[Vessels$NameType == "Ark Royal Tender" ] <- 320
Vessels$PricePP[Vessels$NameType == "Hamdy Cruise LVB" ] <- 418
Vessels$PricePP[Vessels$NameType == "Hamdy Cruise Tender" ] <- 418

# Taking the last word to extract type information
Vessels$Type <- word(Vessels$NameType, start = -1)


# Came up with avg price for each type
AvgPrices <- as.data.frame.table(tapply(Vessels$PricePP, Vessels$Type, mean, na.rm = TRUE))

names(AvgPrices) <- c("Type", "PriceEst")

Vessels <- merge(Vessels, AvgPrices)

Vessels$PriceEst[!is.na(Vessels$PricePP)] <- Vessels$PricePP[!is.na(Vessels$PricePP)]

Vessels <- Vessels[ ,c(-1, -3)]
# Merge the VesselLog with the VesselList

VesselLog <- merge(VesselLog, Vessels)

# Take out the driver
VesselLog$TripPrice <- VesselLog$PriceEst * (VesselLog$PeopleonBoard -1)
VesselLog$PeopleWOCrew <- VesselLog$PeopleonBoard - 1

# PREPARE DATA FRAME ------------------------------------------------------------------

# This takes out the time, and groups the dates by price per day

Daily <- as.data.frame.table(tapply(VesselLog$TripPrice, as.Date(VesselLog$DateTime), sum, na.rm = TRUE))
Daily.People <- as.data.frame.table(tapply(VesselLog$PeopleWOCrew, as.Date(VesselLog$DateTime), sum, na.rm = TRUE))

table(VesselLog$Type, year(VesselLog$DateTime))

names(Daily) <- c("Date", "TotalPrice")
names(Daily.People) <- c("Date", "Guests")
Daily <- merge(Daily, Daily.People)
Daily$Date <- as.Date(Daily$Date)
Daily$Weekday <- weekdays(Daily$Date)

Daily$Month <- months(Daily$Date) 
Daily$Month <- as.factor(Daily$Month)

Daily$Season <- NA
Daily$Season <- "Low"
Daily$Season [month(Daily$Date) == 10 | month(Daily$Date) == 11 | month(Daily$Date) == 12 | month(Daily$Date) == 1 | month(Daily$Date) == 2] <- "High"
Daily$Season <- as.factor(Daily$Season)

Daily$SeasonB <- NA
Daily$SeasonB <- "Low"
Daily$SeasonB [month(Daily$Date) == 12 | month(Daily$Date) == 1 | month(Daily$Date) == 2 | month(Daily$Date) == 3 | month(Daily$Date) == 4] <- "High"
Daily$SeasonB <- as.factor(Daily$SeasonB)

Daily$Year <- as.factor(year(Daily$Date))
Daily$Day <- as.numeric(Daily$Date)-min(as.numeric(Daily$Date))

Daily$Liveaboard <- as.vector(table(as.Date(VesselLog$DateTime), VesselLog$Association)[,1])
Daily$Resort <- as.vector(table(as.Date(VesselLog$DateTime), VesselLog$Association)[,4])
Daily$GuestLive <- as.data.frame(tapply(VesselLog$PeopleonBoard, list(as.Date(VesselLog$DateTime), VesselLog$Association), sum, na.rm = TRUE))$Liveaboard
Daily$GuestResort <- as.data.frame(tapply(VesselLog$PeopleonBoard, list(as.Date(VesselLog$DateTime), VesselLog$Association), sum, na.rm = TRUE))$Resort
Daily$GuestLive[is.na(Daily$GuestLive)] <- 0
Daily$GuestResort[is.na(Daily$GuestResort)] <- 0

Wind <- read.csv('WindData.csv')
Wind$time <- as.Date(as.POSIXct(Wind$time*60*60, origin = '1978-01-01', tz = "Asia/Thimphu"))
Wind$wind.sm [Wind$wind.sm < -30] <- NA
  names(Wind) <- c("WSpeed", "Date")
Daily <- merge (Daily, Wind)

# PRICE MODEL -------------------------------------------------------------

#Build Model
ModelA <- lm (TotalPrice ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily)
ModelB <- lm (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily)
ModelC <- glm (TotalPrice ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, family = "poisson")
ModelD <- glm (TotalPrice ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, family = "quasipoisson")
ModelE <- glm (TotalPrice ~ Weekday + Season + Year + Weekday:Year, data = Daily, family = "quasipoisson")
ModelF <- glm (TotalPrice ~ Weekday + Season + Year, data = Daily, family = "quasipoisson")

ModelG <- gls (TotalPrice ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily)

ModelH <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily) 

Model1A  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + WSpeed, data = Daily, method = "ML") 
Model1  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, method = "ML") 
Model2  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, weights = varComb(varIdent (form= ~ 1 | Year)), method = "ML") 
Model3  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, weights = varComb(varIdent (form= ~ 1 | Season)), method = "ML") 
Model4  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, weights = varComb(varIdent (form= ~ 1 | Weekday)), method = "ML") 
Model5  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season)), method = "ML") 
Model6  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Weekday)), method = "ML") 
Model7  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML")
Model8  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday), varIdent (form= ~ 1 | Year)), method = "ML")
Model9  <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model10 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model11 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 1), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model12 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model13 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 2), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model14 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 2), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model15 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model16 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model17 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")

Model18 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model19 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model20 <- gls (I(log(TotalPrice+1)) ~ Weekday + Year + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
Model21 <- gls (I(log(TotalPrice+1)) ~ Season + Year + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")

# Model19 <- gls (I(log(TotalPrice+1)) ~ Weekday + Year, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
# Model20 <- gls (I(log(TotalPrice+1)) ~ Season + Year, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
# Model21 <- gls (I(log(TotalPrice+1)) ~ Weekday, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
# Model22 <- gls (I(log(TotalPrice+1)) ~ Year, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
# Model23 <- gls (I(log(TotalPrice+1)) ~ Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")

anova(Model1, Model2, Model3, Model4, Model5, Model6, Model7, Model8, Model9, Model10, Model11, Model12, Model13, Model14, Model15, Model16, Model17, Model18, Model19, Model20, Model21)
# Temporal correlation yes
ModelI <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corAR1(form =~ Day), method = "ML") 
ModelI20 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 0), method = "ML") 
ModelI30 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 3, q = 0), method = "ML") 
ModelI11 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 1), method = "ML") 
ModelI21 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), method = "ML") 
ModelI31 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 3, q = 1), method = "ML") 
ModelI12 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 2), method = "ML") 
ModelI22 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 2), method = "ML") 
ModelI32 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 3, q = 2), method = "ML") 
ModelI13 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 3), method = "ML") 
ModelI23 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 3), method = "ML") 
ModelI33 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 3, q = 3), method = "ML") 
anova(ModelI, ModelI20,ModelI30, ModelI11, ModelI21, ModelI31, ModelI12, ModelI22, ModelI32, ModelI13, ModelI23, ModelI33)
# Variance
ModelJ <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday))) 
ModelK <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday))) 

ModelK.ML <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelJ.ML <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corAR1(form =~ Day), weights = varIdent (form= ~ 1 | Year * Season * Weekday), method = "ML") 
ModelI.ML <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corAR1(form =~ Day), method = "ML") 
ModelH.ML <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, method = "ML") 
ModelI.1 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelK.ML.1 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year)), method = "ML") 
ModelK.ML.2 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Season)), method = "ML") 
ModelK.ML.3 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelK.ML.4 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season)), method = "ML") 
ModelK.ML.5 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelK.ML.6 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 

ModelK.ML.1A <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Year)), method = "ML") 
ModelK.ML.2A <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season)), method = "ML") 
ModelK.ML.3A <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelK.ML.4A <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season)), method = "ML") 
ModelK.ML.5A <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelK.ML.6A <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 


# By BIC and for simplification we go for K
ModelK1 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year+ Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 

ModelL <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Year, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelL.1 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + Weekday:Season, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 

ModelM <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year, data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 
ModelN <- gls (I(log(TotalPrice+1)) ~ Weekday + Season , data = Daily, correlation = corARMA(form =~ Day, p = 2, q = 1), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday)), method = "ML") 
anova(ModelL, ModelL.1, ModelM, ModelN)

# Chnging season
ModelN <- gls (I(log(TotalPrice+1)) ~ Weekday + SeasonB + Year, data = Daily, correlation = corAR1(form =~ Day), weights = varComb(varIdent (form= ~ 1 | Year), varIdent (form= ~ 1 | SeasonB), varIdent (form= ~ 1 | Weekday)), method = "ML") 

# PEOPLE MODEL ------------------------------------------------------------

ModelZ <- glm (Guests ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + Season:Year, family = "quasipoisson", data = Daily)
ModelY <- glm (Guests ~ Weekday + Season +Year + Weekday:Season + Weekday:Year, family = "quasipoisson", data = Daily)
ModelX <- glm (Guests ~ Weekday + Season +Year + Weekday:Season, family = "quasipoisson", data = Daily)
ModelX.2 <- glm (Guests ~ Weekday + Season +Year + Weekday:Year, family = "quasipoisson", data = Daily)
ModelW <- glm (Guests ~ Weekday + Season +Year, family = "quasipoisson", data = Daily)
ModelV <- glm (Guests ~ Weekday + Season, family = "quasipoisson", data = Daily)

GLModel1 <- glm (GuestLive ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + I(log(WSpeed + 1)), family = "poisson", data = Daily)
GLModel1A <- glm (GuestLive ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), family = "quasipoisson", data = Daily)
GLModel1B <- glm.nb (GuestLive ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), data = Daily)

GL.AIC <- glmulti(GuestLive ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
coef(GL.AIC, select = 0.95)

GLModelA <- glm (GuestLive ~ Weekday + Season +Year + Weekday:Season + Weekday:Year, family = "quasipoisson", data = Daily)
GLModelB <- glm (GuestLive ~ Weekday + Season +Year + Weekday:Year, family = "quasipoisson", data = Daily)
GLModelB2 <- glm (GuestLive ~ Weekday + Season +Year + Weekday:Season, family = "quasipoisson", data = Daily)
GLModelC <- glm (GuestLive ~ Weekday + Season +Year, family = "quasipoisson", data = Daily)
GLModelD <- glm (GuestLive ~ Weekday + Season, family = "quasipoisson", data = Daily)

GR.AIC <- glmulti(GuestResort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
coef(GR.AIC, select = 0.95)

G.AIC <- glmulti(I(GuestResort + GuestLive) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
coef(GR.AIC, select = 0.95)

GRModelA <- glm (GuestResort ~ Weekday + Season +Year + Weekday:Season + Weekday:Year, family = "quasipoisson", data = Daily)
GRModelB <- glm (GuestResort ~ Weekday + Season +Year + Weekday:Year, family = "quasipoisson", data = Daily)
GRModelB2 <- glm (GuestResort ~ Weekday + Season +Year + Weekday:Season, family = "quasipoisson", data = Daily)
GRModelC <- glm (GuestResort ~ Weekday + Season +Year, family = "quasipoisson", data = Daily)
GRModelD <- glm (GuestResort ~ Weekday + Season, family = "quasipoisson", data = Daily)
GRModelE <- glm (GuestResort ~ Weekday + Season, family = "quasipoisson", data = Daily)
anova(GRModelA, GRModelB,GRModelB2, GRModelC, GRModelD, GRModelE, test = "Chisq")

# LIVEABOARD & RESORT ----------------------------------------------------

LModelA <- glm (Liveaboard ~ Weekday + Season +Year + Weekday:Season + Weekday:Year, family = "quasipoisson", data = Daily)
LModelB <- glm (Liveaboard ~ Weekday + Season +Year + Weekday:Year, family = "quasipoisson", data = Daily)
LModelC <- glm (Liveaboard ~ Weekday + Season +Year + Weekday:Season, family = "quasipoisson", data = Daily)
LModelD <- glm (Liveaboard ~ Weekday + Season +Year , family = "quasipoisson", data = Daily)
LModelE <- glm (Liveaboard ~ Weekday + Season , family = "quasipoisson", data = Daily)
LModelF <- glm (Liveaboard ~ Season , family = "quasipoisson", data = Daily)
anova(LModelA, LModelB, LModelC, LModelD, LModelE, LModelF, test = "Chisq")

L.AIC <- glmulti(Liveaboard ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
coef(L.AIC, select = 0.95)

RModelA <- glm (Resort ~ Weekday + Season +Year + Weekday:Season + Weekday:Year, family = "quasipoisson", data = Daily)
RModelB <- glm (Resort ~ Weekday + Season +Year + Weekday:Year, family = "quasipoisson", data = Daily)
RModelC <- glm (Resort ~ Weekday + Season +Year + Weekday:Season, family = "quasipoisson", data = Daily)
RModelD <- glm (Resort ~ Weekday + Season +Year , family = "quasipoisson", data = Daily)
RModelE <- glm (Resort ~ Weekday + Season  , family = "quasipoisson", data = Daily)
anova(RModelA, RModelB, RModelC, RModelD, RModelE, test = "Chisq")

R.AIC <- glmulti(Resort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
coef(R.AIC, select = 0.95)

B.AIC <- glmulti(I(Resort+Liveaboard) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
coef(R.AIC, select = 0.95)


# LOG LIKELIHOODS ---------------------------------------------------------

LLCompare <- function(Model1, Model2){
  d <- 2 * (logLik(Model2) - logLik(Model1))
  pval <- 0.5 * pchisq(as.numeric(d), df = 1, lower.tail = FALSE)
  return(pval)
}

GLModelA <- glm (GuestLive ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + I(log(WSpeed + 1)), family = "poisson", data = Daily)
GLModelB <- glm.nb (GuestLive ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), data = Daily)
GRModelA <- glm (GuestResort ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + I(log(WSpeed + 1)), family = "poisson", data = Daily)
GRModelB <- glm.nb (GuestResort ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), data = Daily)
GModelA <- glm (I(GuestLive + GuestLive) ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + I(log(WSpeed + 1)), family = "poisson", data = Daily)
GModelB <- glm.nb (I(GuestLive + GuestLive) ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), data = Daily)

BLModelA <- glm (Liveaboard ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + I(log(WSpeed + 1)), family = "poisson", data = Daily)
BLModelB <- glm.nb (Liveaboard ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), data = Daily)
BRModelA <- glm (Resort ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + I(log(WSpeed + 1)), family = "poisson", data = Daily)
BRModelB <- glm.nb (Resort ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), data = Daily)
BModelA <- glm (I(Liveaboard + Resort) ~ Weekday + Season +Year + Weekday:Season + Weekday:Year + I(log(WSpeed + 1)), family = "poisson", data = Daily)
BModelB <- glm.nb (I(Liveaboard + Resort) ~ Weekday + Season +Year + Weekday:Season + Weekday:Year+ I(log(WSpeed + 1)), data = Daily)

LLCompare (GLModelA, GLModelB)
LLCompare (GRModelA, GRModelB)
LLCompare (GModelA, GModelB)
LLCompare (BLModelA, BLModelB)
LLCompare (BRModelA, BRModelB)
LLCompare (BModelA, BModelB)

Residuals <- data.frame(Date = Daily$Date, GLR = residuals(BLModelB), GRR = residuals(GRModelB), GR = residuals(GModelB), BLR = residuals(BLModelB), BRR = residuals(BRModelB), BR = residuals(BModelB))
DateResiduals <- data.frame (Date = seq(as.Date("2011-01-01"), as.Date("2013-12-31"), by = "day"))
DateResiduals <- merge (DateResiduals, Residuals, all.x = TRUE)
acf(DateResiduals$BR, na.action = na.pass)

# PREDICTIONS -------------------------------------------------------------

Model17G <- Gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year, data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
ModelWG <- Glm (Guests ~ Weekday + Season +Year, family = "quasipoisson", data = Daily)
LModelE <- glm (Liveaboard ~ Weekday + Season , family = "quasipoisson", data = Daily)
RModelD <- glm (Resort ~ Weekday + Season +Year , family = "quasipoisson", data = Daily)


# Data frame for Predicting Value
DatesForPrediction <- seq(as.Date("2012-01-01"), as.Date("2013-12-31"), by = "day")
Prediction <- data.frame(Date = DatesForPrediction,
                         Weekday = weekdays (DatesForPrediction),
                         Year = as.factor(year(DatesForPrediction)))
Prediction$Season <- NA
Prediction$Season <- "Low"
Prediction$Season [month(Prediction$Date) == 10 | month(Prediction$Date) == 11 | month(Prediction$Date) == 12 | month(Prediction$Date) == 1 | month(Prediction$Date) == 2] <- "High"
Prediction$Season <- as.factor(Prediction$Season)

Pred <- merge(Prediction, Wind)
Pred$WSpeed[is.na(Pred$WSpeed)] <- mean(Pred$WSpeed, na.rm = TRUE)
Prediction<- Pred

Prediction$EstPrice <- exp(predict(Model17, Prediction))
#Prediction$EstPriceL <- exp(predict(Model17G, Prediction, conf.int = 0.95)$lower)
#Prediction$EstPriceU <- exp(predict(Model17G, Prediction, conf.int = 0.95)$upper)
#Prediction$EstPriceSEL <- Prediction$EstPrice - exp(1.96*predict(Model17G, Prediction, se.fit = TRUE)$se)
#Prediction$EstPriceSEU <- Prediction$EstPrice + exp(1.86*predict(Model17G, Prediction, se.fit = TRUE)$se)


Prediction$EstGuests <- as.vector(predict(G.AIC, newdata = Prediction, type = "response")$averages)
#Prediction$EstGuestsU <- exp(predict(ModelWG, Prediction, conf.int = 0.95, type = "lp")$upper)
#Prediction$EstGuestsL <- exp(predict(ModelWG, Prediction, conf.int = 0.95, type = "lp")$lower)


PredictionB <- rbind(Prediction, Prediction)
PredictionB$Type <- rep (c("Liveaboard", "Resort"), each = nrow(Prediction))
PredictionB$Boats[PredictionB$Type == "Liveaboard"] <- as.vector (predict(BL.AIC, newdata = Prediction, type = "response")$averages)
PredictionB$Boats[PredictionB$Type == "Resort"] <- as.vector (predict(BR.AIC, newdata = Prediction, type = "response")$averages)

Yearly <- as.data.frame.table(table(VesselLog$Association, year(VesselLog$DateTime)))
names(Yearly) <- c("Association", "Year", "Count")
Aux <- as.data.frame.table(table(year(unique(as.Date(VesselLog$DateTime)))))
names(Aux) <- c("Year", "Surveys")
Yearly <- merge(Yearly, Aux)
Yearly$CountAdjusted <- Yearly$Count / Yearly$Surveys

Year <- data.frame(Year = c(2011, 2012, 2013), Exp = tapply(Prediction$EstPrice, Prediction$Year, sum), ExpL = tapply(Prediction$EstPriceL, Prediction$Year, sum), ExpU = tapply(Prediction$EstPriceU, Prediction$Year, sum))
Year$People <- tapply(Prediction$EstGuests, Prediction$Year, sum)
Year$PeopleL <- tapply(Prediction$EstGuestsL, Prediction$Year, sum)
Year$PeopleU <- tapply(Prediction$EstGuestsU, Prediction$Year, sum)


Prediction$Weekday <- factor(Prediction$Weekday, levels = c ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") )
PredictionB$Weekday <- factor(PredictionB$Weekday, levels = c ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday") )


P <- ggplot(Prediction)
# png("GuestWeek.png", width = 200, height = 200/sqrt(2))
# P+ geom_boxplot(aes(x = Weekday, y = EstGuests))
# dev.off()
# png("GuestSeason.png", width = 200, height = 200/sqrt(2))
# P+ geom_boxplot(aes(x = Season, y = EstGuests))
# dev.off()
# png("GuestYear.png", width = 200, height = 200/sqrt(2))
# P+ geom_boxplot(aes(x = Year, y = EstGuests))
# dev.off()
pdf("BoatWeek.pdf", width = 5.4306/2, height = 5.4306/sqrt(2)/2)
ggplot(PredictionB)+ geom_boxplot(aes(x = Weekday, y = Boats, fill = Type))  + theme_bw() + theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) + ylab("Number of boats")
dev.off()
pdf("BoatSeason.pdf", width = 5.4306/2, height = 5.4306/sqrt(2)/2)
ggplot(PredictionB)+ geom_boxplot(aes(x = Season, y = Boats, fill = Type)) + theme_bw () + theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + ylab("Number of boats")
dev.off()
# png("BoatYear.png", width = 200, height = 200/sqrt(2))
# ggplot(PredictionB)+ geom_boxplot(aes(x = Year, y = Boats, fill = Type)) + theme(legend.position = "none")
# dev.off()
pdf("ExpendWeek.pdf", width = 5.4306/2, height = 5.4306/sqrt(2)/2)
P+ geom_boxplot(aes(x = Weekday, y = EstPrice)) + theme_bw() + theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) + ylab("Est. Expenditure") + scale_y_continuous(breaks = c(0,25000,50000,75000), labels = c("0", "25.0", "50.0", "75.0"))
dev.off()
pdf("ExpendSeason.pdf",  width = 5.4306/2, height = 5.4306/sqrt(2)/2)
P+ geom_boxplot(aes(x = Season, y = EstPrice)) + theme_bw() + theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + ylab("Est. Expenditure") + scale_y_continuous(breaks = c(0,25000,50000,75000), labels = c("0", "25.0", "50.0", "75.0"))
dev.off()
# png("ExpendYear.png", width = 200, height = 200/sqrt(2))
# P+ geom_boxplot(aes(x = Year, y = EstPrice))
# dev.off()

ggplot(Year) + geom_bar (aes(x = as.factor(Year), y = Exp), stat = "identity")

# SPATIAL DISTRIBUTION ----------------------------------------------------

library(ggmap)
library (sp)

Sight <- data.frame(Lat = VesselLog$Lattitude, Lon = VesselLog$Longitude)
Sight <- Sight[!is.na (Sight$Lat), ]  # Delete NAs
Sight <- Sight[!is.na (Sight$Lon), ]

# Read Survey track
Survey <- read.csv ("SurveyTrack.csv", skip = 42)  
# Only keep columns with useful information
Survey <- Survey[, c (4, 3)]; names (Survey) <- c ("Lon", "Lat")
Survey$Lon <- as.numeric (as.character (Survey$Lon))
SurveyCM <- as.matrix (Survey)

# Calculate distances of survey
Aux.Distances <- spDists (SurveyCM, SurveyCM, longlat = TRUE)  
Aux.Distances <- Aux.Distances[, -1]
Survey <- Survey[-1, ]  # Delete first reading
Survey$Cum.Dist <- cumsum (diag (Aux.Distances))

# Locate sightings on survey
SightCM <- as.matrix (cbind (Sight$Lon, Sight$Lat))
SurveyCM <- as.matrix (Survey [, c (1,2)])
Aux.Distances <- spDists (SightCM, SurveyCM, longlat = TRUE)
Aux.DistancesB <- max (Aux.Distances) - Aux.Distances
Aux.MinDistInd <- max.col (Aux.DistancesB)
Sight$Dist.To.Survey <- apply (Aux.Distances, 1, min)
Sight$Dist.On.Survey <- Survey[Aux.MinDistInd, 3]

## Read survey effort file
Effort <- read.csv ("Clean Survey Tracks to 2014-04-01.csv", skip = 186)
Effort <- Effort [, c(4, 3, 6)]; names (Effort) <- c ("Lon", "Lat", "Date")
Effort$Date <- as.POSIXct (Effort$Date, format = "%Y-%m-%dT%XZ", tz = "Asia/Thimphu")
Effort$Lon <- as.numeric (as.character(Effort$Lon))

# Compensate for 30 second period 
Effort$thirtysec <- FALSE
for (i in 1:nrow(Effort)){
  if (i %% 2 == 0){
    if (as.numeric (Effort$Date[i])- as.numeric (Effort$Date[i-1]) == 30){
      Effort$thirtysec[i] <- TRUE
    }      
  }
}
Effort <- Effort [Effort$thirtysec == FALSE, ]

# Locate effort positions on survey 
EffortCM <- as.matrix (cbind (Effort$Lon, Effort$Lat))
Aux.Distances <- spDists (EffortCM, SurveyCM, longlat = TRUE)
Aux.DistancesB <- max (Aux.Distances) - Aux.Distances
Aux.MinDistInd <- max.col (Aux.DistancesB)
Effort$Dist.To.Survey <- apply (Aux.Distances, 1, min)
Effort$Dist.On.Survey <- Survey[Aux.MinDistInd, 3]

# Delete observations outside SAMPA
Sight <- Sight[Sight$Dist.To.Survey <= 0.5, ]
Effort <- Effort [Effort$Dist.To.Survey <= 0.5, ]

# Marks
Marks <- data.frame (Lon = approx(Survey$Cum.Dist, Survey$Lon, xout = seq(from = 0, to = 43, by = 5))$y,
                     Lat = approx(Survey$Cum.Dist, Survey$Lat, xout = seq(from = 0, to = 43, by = 5))$y,
                     Dist.On.Survey = seq(from = 0, to = 43, by = 5))

# Delete the first 5km on the surveys
Survey <- Survey[Survey$Cum.Dist > 5,]
Survey$Cum.Dist <- Survey$Cum.Dist - 5
# In the sightings data frame
Sight$Dist.On.Survey <- Sight$Dist.On.Survey - 5
Sight$Cum.Dist.On.Survey <- cumsum(Sight$Dist.On.Survey)
# In the effort data frame
Effort$Dist.On.Survey <- Effort$Dist.On.Survey - 5
Effort <- Effort[Effort$Dist.On.Survey >= 0, ]
Effort$Cum.Dist.On.Survey <- cumsum(Effort$Dist.On.Survey)


map <- get_map (location = c(72.7, 3.45, 72.95, 3.6), maptype = "terrain", source = 'osm', zoom = 11)
png ("Map.png", width = 300, height = 300/sqrt(2))
ggmap (map, darken = c(0.5, 'white')) + 
  geom_path (aes (x = Lon, y = Lat), data = Survey, linetype = 4, colour = "red", alpha = 0.5) +
#  geom_jitter (aes(x = Lon, y = Lat),alpha = 0.2, size = 2, data = Sight, position = position_jitter(width = 0.005, height = 0.005)) +
  geom_point (aes(x = Lon, y = Lat), colour = "Red", data = Marks, shape = 3)
dev.off()
# 1D Plot
pdf ("./Figures/FrequencyObs2.pdf", width = 5.4306, height = 5.4306/sqrt(2)/2)
ggplot() + geom_density (aes (x = Dist.On.Survey, y = ..scaled..), adjust = 1/2, binwidth = 1, fill = "white", data = Sight) + 
  geom_density (aes (x = Dist.On.Survey, y = ..scaled..), adjust = 1/2, binwidth = 1, fill = "black", data = Effort, alpha = 0, linetype = 2)+ theme_bw()
#  geom_point(data = Marks, aes(x = Dist.On.Survey, y = 0.025), colour = "Red", shape = 3)
dev.off()

#index <- Effort$Date <= as.POSIXct ("2014-01-01 00:00:00") & yday (Effort$Date) != yday(as.POSIXct("2013-12-11 00:00:00")) & yday (Effort$Date) != yday(as.POSIXct("2013-12-25 00:00:00"))
index <- TRUE

plot <- ggplot() + geom_density (aes (x = Dist.On.Survey, y = ..scaled..), adjust = 1/2, binwidth = 1, fill = "black", data = Sight, alpha = 0.5) + 
  geom_density (aes (x = Dist.On.Survey, y = ..scaled..), adjust = 1/2, binwidth = 1, fill = "black", data = subset (Effort, index ), alpha = 0.5)+ theme_bw()
plot <- print (plot)

density <- data.frame (x = plot$data[[1]]$x, boats = plot$data[[1]]$scaled, effort = plot$data[[2]]$scaled)
boat.effort.model <- glm (boats ~ effort, data = density, family = "binomial")
density$residuals <- residuals (boat.effort.model, type = "response")
density$residuals.scaled <- with(density, (residuals - min (residuals) + min(boats)) / (max(residuals) - min (residuals) + min(boats))) 

# Plot with corrected boat density
pdf ("./Figures/FrequencyObs3.pdf", width = 5.4306, height = 5.4306/sqrt(2)/2)
ggplot (density, aes(x = x)) + geom_area (aes (y = residuals.scaled), fill = "white", colour = "black") +
  geom_line(aes (y = boats), linetype = 2) + 
  geom_line (aes (y = effort), linetype = 3) + theme_bw()
dev.off()

ggplot() + geom_density (aes (x = Dist.On.Survey, y = cumsum(..density..)/sum(..density..)), adjust = 1/2, binwidth = 1, fill = "black", alpha = 0.2, data = Sight) + 
  geom_density (aes (x = Dist.On.Survey, y = cumsum(..density..)/sum(..density..)), adjust = 1/2, binwidth = 1, fill = "black", alpha = 0.2, data = Effort) 

qplot(x = x, y = cumsum(residuals.scaled) / 250, data = density)

100/8

# BOOTSRAPPING ------------------------------------------------------------
DatesForPrediction <- seq(as.Date("2011-01-01"), as.Date("2013-12-31"), by = "day")
Prediction <- data.frame(Date = DatesForPrediction,
                         Weekday = weekdays (DatesForPrediction),
                         Year = as.factor(year(DatesForPrediction)))
Prediction$Season <- NA
Prediction$Season <- "Low"
Prediction$Season [month(Prediction$Date) == 10 | month(Prediction$Date) == 11 | month(Prediction$Date) == 12 | month(Prediction$Date) == 1 | month(Prediction$Date) == 2] <- "High"
Prediction$Season <- as.factor(Prediction$Season)
Pred <- merge(Prediction, Wind)
Pred$WSpeed[is.na(Pred$WSpeed)] <- mean(Pred$WSpeed, na.rm = TRUE)

rep <- 2
Bootstrapi <- data.frame(GL2011 = 1:rep, GL2012 = 1:rep, GL2013 = 1:rep, GR2011 = 1:rep, GR2012 = 1:rep, GR2013 = 1:rep, G2011 = 1:rep, G2012 = 1:rep, G2013 = 1:rep,
                        BL2011 = 1:rep, BL2012 = 1:rep, BL2013 = 1:rep, BR2011 = 1:rep, BR2012 = 1:rep, BR2013 = 1:rep, B2011 = 1:rep, B2012 = 1:rep, B2013 = 1:rep)

for (i in 1:rep){
  t1<- Sys.time()
  #P <- Daily[sample(1:nrow(Daily),nrow(Daily), replace = TRUE), ]
  P <- Daily
  YearlyGL <- tryCatch({
    GL.AIC <- glmulti(GuestLive ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, fitfunction = glm.nb, marginality = TRUE, data = P)
    GL <- as.vector(predict(GL.AIC, newdata = Pred, type = "response")$averages)
    YearlyGL <- as.vector(tapply(GL, Pred$Year, sum))}, error = function(err) return(c(NA,NA,NA)))
  
  YearlyGR <- tryCatch({
    GR.AIC <- glmulti(GuestResort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, fitfunction = glm.nb, marginality = TRUE, data = P)
    GR <- as.vector(predict(GR.AIC, newdata = Pred, type = "response")$averages)
    YearlyGR <- as.vector(tapply(GR, Pred$Year, sum))}, error = function(err) return(c(NA,NA,NA)))
  
  YearlyG <- tryCatch({
    G.AIC <- glmulti(I(GuestLive + GuestResort) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, fitfunction = glm.nb, marginality = TRUE, data = P)
    G <- as.vector(predict(G.AIC, newdata = Pred, type = "response")$averages)
    YearlyG <- as.vector(tapply(G, Pred$Year, sum))}, error = function(err) return(c(NA,NA,NA)))
  
  YearlyBL <- tryCatch({
    BL.AIC <- glmulti(Liveaboard ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, fitfunction = glm.nb, marginality = TRUE, data = P)
    BL <- as.vector(predict(BL.AIC, newdata = Pred, type = "response")$averages)
    YearlyBL <- as.vector(tapply(BL, Pred$Year, sum))}, error = function(err) return(c(NA,NA,NA)))
  
  YearlyBR <- tryCatch({
    BR.AIC <- glmulti(Resort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, fitfunction = glm.nb, marginality = TRUE, data = P)
    BR <- as.vector(predict(BR.AIC, newdata = Pred, type = "response")$averages)
    YearlyBR <- as.vector(tapply(BR, Pred$Year, sum))}, error = function(err) return(c(NA,NA,NA)))
  
  YearlyB <- tryCatch({
    B.AIC <- glmulti(I(Liveaboard + Resort) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, fitfunction = glm.nb, marginality = TRUE, data = P)
    B <- as.vector(predict(B.AIC, newdata = Pred, type = "response")$averages)
    YearlyB <- as.vector(tapply(B, Pred$Year, sum))}, error = function(err) return(c(NA,NA,NA)))
  
#   GR.AIC <- glmulti(GuestResort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = P, fitfunction = glm.nb, marginality = TRUE)
#   G.AIC <- glmulti(I(GuestResort + GuestLive) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = P, fitfunction = glm.nb, marginality = TRUE)
#   BL.AIC <- glmulti(Liveaboard ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = P, fitfunction = glm.nb, marginality = TRUE)
#   BR.AIC <- glmulti(Resort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = P, fitfunction = glm.nb, marginality = TRUE)
#   B.AIC <- glmulti(I(Resort+Liveaboard) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = P, fitfunction = glm.nb, marginality = TRUE)
#   
#   GL <- tryCatch(as.vector(predict(GL.AIC, newdata = Pred, select = 0.95, type = "response")$averages), error = function(err) return(as.vector(predict(GL.AIC, newdata = Pred, select = 0.95, type = "response"))))
#   GR <- tryCatch(as.vector(predict(GR.AIC, newdata = Pred, select = 0.95, type = "response")$averages), error = function(err) return(as.vector(predict(GR.AIC, newdata = Pred, select = 0.95, type = "response"))))
#   G <- tryCatch(as.vector(predict(G.AIC, newdata = Pred, select = 0.95, type = "response")$averages), error = function(err) return(as.vector(predict(G.AIC, newdata = Pred, select = 0.95, type = "response"))))
#   BL <- tryCatch(as.vector(predict(BL.AIC, newdata = Pred, select = 0.95, type = "response")$averages), error = function(err) return(as.vector(predict(BL.AIC, newdata = Pred, select = 0.95, type = "response"))))
#   BR <- tryCatch(as.vector(predict(BR.AIC, newdata = Pred, select = 0.95, type = "response")$averages), error = function(err) return(as.vector(predict(BR.AIC, newdata = Pred, select = 0.95, type = "response"))))
#   B <- tryCatch(as.vector(predict(B.AIC, newdata = Pred, select = 0.95, type = "response")$averages), error = function(err) return(as.vector(predict(B.AIC, newdata = Pred, select = 0.95, type = "response"))))
#   
#   YearlyGL <- as.vector(tapply(GL, Pred$Year, sum))
#   YearlyGR <- as.vector(tapply(GR, Pred$Year, sum))
#   YearlyG <- as.vector(tapply(G, Pred$Year, sum))
#   YearlyBL <- as.vector(tapply(BL, Pred$Year, sum))
#   YearlyBR <- as.vector(tapply(BR, Pred$Year, sum))
#   YearlyB <- as.vector(tapply(B, Pred$Year, sum))
  
  Bootstrapi[i, ] <- c(YearlyGL, YearlyGR, YearlyG, YearlyBL, YearlyBR, YearlyB)
  print(difftime(Sys.time(), t1))
}


print(YearlyGL)


ExpBoot <- function (data, i){
  print(i)
  GL.AICs <- glmulti(GuestLive ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, fitfunction = glm.nb, marginality = TRUE, data = data, trace = TRUE, subset = i)
  print(GL.AICs)
  GR.AIC <- glmulti(GuestResort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
  G.AIC <- glmulti(I(GuestResort + GuestLive) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
  BL.AIC <- glmulti(Liveaboard ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
  BR.AIC <- glmulti(Resort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
  B.AIC <- glmulti(I(Resort+Liveaboard) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
GL <- as.vector(predict(GL.AICs, newdata = Pred, select = 0.95, type = "response")$averages)
print(summary(GL))
  Pred$GR <- as.vector(predict(GR.AIC, newdata = Pred, select = 0.95, type = "response")$averages)
  Pred$G <- as.vector(predict(G.AIC, newdata = Pred, select = 0.95, type = "response")$averages)
  Pred$BL <- as.vector(predict(BL.AIC, newdata = Pred, select = 0.95, type = "response")$averages)
  Pred$BR <- as.vector(predict(BR.AIC, newdata = Pred, select = 0.95, type = "response")$averages)
  Pred$B <- as.vector(predict(B.AIC, newdata = Pred, select = 0.95, type = "response")$averages)
  YearlyGL <- as.vector(tapply(GL, Pred$Year, sum))

  YearlyGR <- as.vector(tapply(Pred$GR, Pred$Year, sum))
  YearlyG <- as.vector(tapply(Pred$G, Pred$Year, sum))
  YearlyBL <- as.vector(tapply(Pred$BL, Pred$Year, sum))
  YearlyBR <- as.vector(tapply(Pred$BR, Pred$Year, sum))
  YearlyB <- as.vector(tapply(Pred$B, Pred$Year, sum))
  message("Processing...")
  #return(c(YearlyGL, YearlyGR, YearlyG, YearlyBL, YearlyBR, YearlyB))
  return(c(YearlyGL))
} 

system.time(Bootstrap <- boot(data = Daily, statistic = ExpBoot, R = 5, stype = "i", sim = "balanced"))
#boot.ci(Bootstrap, index = 1, type = "norm")

Jack2011 <- function (i,data, Pred){
  Daily <- data[i, ]
  Prediction <- Pred
  Model17 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
  Prediction$EstPrice <- exp(predict(Model17, Prediction))
  YearlyPrice <- as.vector(tapply(Prediction$EstPrice, Prediction$Year, sum))
  message("Processing...")
  return(YearlyPrice[1])
  #return(c(YearlyGuest, YearlyGuestL, YearlyGuestR))  
} 

Jack2012 <- function (i,data, Pred){
  Daily <- data[i, ]
  Prediction <- Pred
  Model17 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
  Prediction$EstPrice <- exp(predict(Model17, Prediction))
  YearlyPrice <- as.vector(tapply(Prediction$EstPrice, Prediction$Year, sum))
  message("Process
## NUMBER OF INDIVIDUALS ACOSUSTICALLY DETECTED CORRECTED BY EFFORT

weeks <- sort (as.Date (cut (ARRAY.EVENTS$DATETIME, 'week')))
ARRAY.WEEK <- expand.grid (DATE.WEEKS = seq (min(weeks), max(weeks), 'week'), RECEIVERID = levels (ARRAY.EVENTS$RECEIVERID), RECORDING = FALSE)

# Read stations file and assign detections to stations
# Assign station and location
for (i in length (ARRAY.EVENTS [,1]:1)){  # For each event
  # If is a retrieval change the recording status to true for the past
  if (ARRAY.EVENTS$EVENT[i] == "RET"){  
    message ("    Analyzing retrieval of ", ARRAY.EVENTS$RECEIVERID[i], " on ", 
             floor_date (ARRAY.EVENTS$DATETIME[i], "day"), " at   Station ", ARRAY.EVENTS$STATIONNAME[i])
    replace.index <- (as.character (ARRAY.EVENTS$RECEIVERID[i]) == as.character(ARRAY.WEEK$RECEIVERID)) & (ARRAY.WEEK$DATE.WEEKS <= as.Date(ARRAY.EVENTS$DATETIME[i]))
    # Insert Record
    ARRAY.WEEK$RECORDING [replace.index] <- TRUE
  }
  # If is a deployment change the recording status to false for the past
  else {  
    message ("    Analyzing deployment  of ", ARRAY.EVENTS$RECEIVERID[i], " on ", 
             floor_date (ARRAY.EVENTS$DATETIME[i], "day"), " from Station ", ARRAY.EVENTS$STA[i])
    replace.index <- (as.character (ARRAY.EVENTS$RECEIVERID[i]) == as.character(ARRAY.WEEK$RECEIVERID)) & (ARRAY.WEEK$DATE.WEEKS < as.Date(ARRAY.EVENTS$DATETIME[i]))
    # Insert Record
    ARRAY.WEEK$RECORDING [replace.index] <- FALSE
  }
}
ing...")
  return(YearlyPrice[2])
  #return(c(YearlyGuest, YearlyGuestL, YearlyGuestR))  
} 

Jack2013 <- function (i,data, Pred){
  Daily <- data[i, ]
  Prediction <- Pred
  Model17 <- gls (I(log(TotalPrice+1)) ~ Weekday + Season + Year + I(log(WSpeed+1)), data = Daily, correlation = corARMA(form =~ Day, p = 1, q = 0), weights = varComb(varIdent (form= ~ 1 | Season), varIdent (form= ~ 1 | Weekday),varIdent (form= ~ 1 | Year)), method = "ML")
  Prediction$EstPrice <- exp(predict(Model17, Prediction))
  YearlyPrice <- as.vector(tapply(Prediction$EstPrice, Prediction$Year, sum))
  message("Processing...")
  return(YearlyPrice[3])
  #return(c(YearlyGuest, YearlyGuestL, YearlyGuestR))  
} 

jacksito2011 <- jackknife (1:nrow(Daily), Jack2011, Daily, Pred)
jacksito2012 <- jackknife (1:nrow(Daily), Jack2012, Daily, Pred)
jacksito2013 <- jackknife (1:nrow(Daily), Jack2013, Daily, Pred)

save (Bootstrap, file = "BootstrapResults2.Rdata")
save (jacksito2011, jacksito2012, jacksito2013, file = "JacknifeResults2.Rdata")

load ("BootstrapResults2.Rdata")
load ("JacknifeResults2.Rdata")
jacksito2011

BR <- Bootstrap
BR[sapply(BR, is.infinite)] <- NA
BR.CI <- data.frame(median = 1:ncol(BR), lci = 1:ncol(BR), uci = 1:ncol(BR))
rownames(BR.CI) <- names(BR)

for (i in 1:ncol(BR)){
  boxplot(BR[,i])
  BR[BR[,i] %in% boxplot.stats(BR[ , i], coef = 10)$out, i] <- NA  #remove extreme outliers
  BR.CI[i,]<- c(median(BR[,i], na.rm = TRUE), approx(cumsum(hist(BR[,i], 200)$counts)/sum(hist(BR[,i], 200)$counts),hist(BR[,i], 200)$mids, xout=c(0.025, 0.975))$y)
}


# ORGANIZE MODEL SELECTION RESULTS ----------------------------------------

GL.AIC <- glmulti(GuestLive ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
GR.AIC <- glmulti(GuestResort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
G.AIC <- glmulti(I(GuestResort + GuestLive) ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
BL.AIC <- glmulti(Liveaboard ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)
BR.AIC <- glmulti(Resort ~ Weekday + Season + Year + I(log(WSpeed + 1)) + Weekday:Season + Weekday:Year, data = Daily, fitfunction = glm.nb, marginality = TRUE)


# Model selection
ExtrForm <- function (Mod){
  as.character(lapply(str_split(as.character(Mod@formulas), "~"), function(x) x[-1]))
}

AICs <- data.frame(form = ExtrForm(G.AIC), G = G.AIC@crits)
AICs$GL <- GL.AIC@crits[match (as.character(AICs$form), ExtrForm(GL.AIC))]
AICs$GR <- GR.AIC@crits[match (as.character(AICs$form), ExtrForm(GR.AIC))]
AICs$BL <- BL.AIC@crits[match (as.character(AICs$form), ExtrForm(BL.AIC))]
AICs$BR <- BR.AIC@crits[match (as.character(AICs$form), ExtrForm(BR.AIC))]


Format <- matrix(rep("", NROW(AICs) * NCOL(AICs)), nrow=NROW(AICs))
Format[match (as.character(AICs$form), ExtrForm(G.AIC)) < 7 ,2] <- "bf"
Format[match (as.character(AICs$form), ExtrForm(GL.AIC)) < 7 ,3] <- "bf"
Format[match (as.character(AICs$form), ExtrForm(GR.AIC)) < 11 ,4] <- "bf"
Format[match (as.character(AICs$form), ExtrForm(BL.AIC)) < 5 ,5] <- "bf"
Format[match (as.character(AICs$form), ExtrForm(BR.AIC)) < 12 ,6] <- "bf"

DAICs <- t(apply(AICs[,-1], 1, function(x) x-apply(AICs[,-1], 2, min)))
Format <- Format[,-1]
AICs$form <- sedit(as.character(AICs$form), c('Year', 'Weekday', 'Season', 'I(log(WSpeed + 1))', '1 + '), c('y','w','s','u', ''))
row.names(DAICs) <- AICs$form
latex.default(DAICs, digits = 3, cellTexCmds = Format, numeric.dollar = FALSE, math.row.names = TRUE)

# Model results

Coefi <- coef(G.AIC, select = 0.95)
ord <- c(10, 11, 12, 16, 17,15, 13, 14, 9, 7, 8, 1, 5, 6, 4, 2, 3)
Coefi <- Coefi[ord, ]
Coefi <- data.frame(Coef = rownames(Coefi), GE = Coefi[,1], GV = Coefi[,2], GI = Coefi[,4])

ExtractCoef <- function (Model){
  CGL <- coef(Model, select = 0.95)
  CGL <- CGL[match(as.character(Coefi$Coef), rownames(CGL)), ]
  return(data.frame(E = CGL[, 1], V = CGL[,2], I = CGL[, 4]))
}

Coefi <- cbind (Coefi, ExtractCoef(GL.AIC), ExtractCoef(GR.AIC), ExtractCoef(BL.AIC), ExtractCoef(BR.AIC))
rownames (Coefi) <- sedit (rownames (Coefi), 
                           c ('WeekdayMonday', 'WeekdayTuesday', 'WeekdayWednesday', 'WeekdayThursday', 'WeekdaySaturday', 'WeekdaySunday', 'SeasonLow', 'I(log(WSpeed + 1))', 'Year'), 
                           c ('Mon', 'Tue', 'Wed', 'Thu', 'Sat', 'Sun', 'Low', 'Wind Speed^*', ''))
latex (Coefi[,-1], dec = 2, math.row.names = TRUE, scientific = c(-3,3), numeric.dollar = FALSE, blank.dot = TRUE)

# Model selection 

GetThetas <- function(Model){
  thetas <- unlist(lapply(Model@objects, getElement, 'theta'))
  return(thetas[1:which.min(abs(cumsum(weightable(Model)$weights) -0.95))])
}

Thetas <- list (summary (GetThetas (G.AIC)), summary(GetThetas(GL.AIC)), summary(GetThetas(GR.AIC)), summary(GetThetas(BL.AIC)), summary(GetThetas(BR.AIC)))
Thetas <- data.frame(Min = unlist (lapply (Thetas, getElement, 1)), Mean = unlist (lapply (Thetas, getElement, 4)), Max = unlist (lapply (Thetas, getElement, 6)))
row.names(Thetas) <- c("Total Guests", "Liveaboard Guests", "Resort Guests", "Boats Liveaboards", "Boats Resorts")

latex(Thetas, numeric.dollar = FALSE)

# Price ranges

VE <- Vessels
VE$Type <- as.factor(VE$Type)
levels(VE$Type) <- c("LFV", "L", "L", "Other", "RDD", "RED", "RSB", "RSFB", "L")

PriceSummary <- data.frame(min = tapply(VE$PricePP, VE$Type, min, na.rm = TRUE),
                           mean = tapply(VE$PricePP, VE$Type, mean, na.rm = TRUE),
                           sd = tapply(VE$PricePP, VE$Type, sd, na.rm = TRUE),
                           max = tapply(VE$PricePP, VE$Type, max, na.rm = TRUE))
row.names(PriceSummary) <- names(tapply(VE$PricePP, VE$Type, sd, na.rm = TRUE))

#Remove other categories
PriceSummary <- PriceSummary[c(-1, -3, -7), ]

latex(PriceSummary, dec = 0)

# Temporal effort

temp.effort.count <- acast (Daily, Year ~ Weekday ~ Season, margins = TRUE, value.var = "Date")
latex (temp.effort.count[, , 2])

## Revision plots ----------------------------

library(plyr)
# Convert into monthly representations

Guest.check <- melt (merge (Daily[, c (1, 3)], Prediction[, c (1, 7)], all = TRUE), "Date", na.rm = TRUE)
Price.check <- melt (merge (Daily[, c (1, 2)], Prediction[, c (1, 6)], all = TRUE), "Date", na.rm = TRUE)
Boat.check <- melt ( merge (data.frame (Date = Daily$Date, TBoat = Daily$Liveaboard + Daily$Resort), 
                            ddply (PredictionB, .(Date), summarise, EstTBoat = sum (Boats)), all = TRUE), "Date", na.rm = TRUE)

pdf ("./Figures/GuestBoxplot.pdf", width = 5.4306, height = 5.4306/sqrt(2)/2)
ggplot(Guest.check, aes (x  = cut(Date, breaks = "month"), y = value)) + 
  geom_boxplot(aes(fill = variable), position = "dodge") + theme_classic() + 
  theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + 
  ylab("Number of guests") + xlab ("Date") + scale_y_continuous (breaks = c (0, 200, 400, 600), labels = c ("0", "20", "20", "20")) +
  scale_fill_manual (values = c("#F2F2F2", "#B3B3B3")) + scale_x_discrete(labels = month(seq(as.Date("2011-11-01"), as.Date("2013-12-01"), by = "month"), label = TRUE))
dev.off()

pdf ("./Figures/PriceBoxplot.pdf", width = 5.4306, height = 5.4306/sqrt(2)/2)
ggplot(Price.check, aes (x = cut(Date, breaks = "month"), y = value)) + 
  geom_boxplot(aes(fill = variable)) + theme_classic() + 
  theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + 
  ylab("Daily expenditure") + xlab ("Date") + scale_y_continuous (breaks = c (0, 50000, 100000), labels = c ("0", "50", "10")) +
  scale_fill_manual (values = c("#F2F2F2", "#B3B3B3")) + scale_x_discrete(labels = month(seq(as.Date("2011-11-01"), as.Date("2013-12-01"), by = "month"), label = TRUE))
dev.off()

pdf ("./Figures/BoatBoxplot.pdf", width = 5.4306, height = 5.4306/sqrt(2)/2)
ggplot(Boat.check, aes (x = cut(Date, breaks = "month"), y = value)) + geom_boxplot(aes(fill = variable)) + theme_classic() + 
  theme(legend.position = "none", panel.grid = element_blank(), panel.border = element_blank(), axis.line = element_line()) + 
  ylab("Number of boats") + xlab ("Date") +
  scale_fill_manual (values = c("#F2F2F2", "#B3B3B3"))+ scale_x_discrete(labels = month(seq(as.Date("2011-11-01"), as.Date("2013-12-01"), by = "month"), label = TRUE))
dev.off()


# Export data -------------------------------------------------------------

VesselLogBook <- VesselLog  [,c(-1,-2, -8, -9, -10,-13,-14, -15)]
write.csv (VesselLogBook, file = "VesselLogBook.csv")
write.csv (Vessels, file = "Vessels.csv")
write.csv (Daily[,-7], file = "DailySummary.csv")
