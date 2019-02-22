UTM2LatLong<-function(UTMX,UTMY){
  
  DatumEqRad = c(6378137.0,6378137.0,6378137.0,6378135.0,6378160.0,6378245.0,6378206.4,
                 6378388.0,6378388.0,6378249.1,6378206.4,6377563.4,6377397.2,6377276.3);	
  DatumFlat = c(298.2572236, 298.2572236, 298.2572215,	298.2597208, 298.2497323, 298.2997381, 294.9786982,
                296.9993621, 296.9993621, 293.4660167, 294.9786982, 299.3247788, 299.1527052, 300.8021499); 
  Item = 1;#Default
  k0 = 0.9996;#scale on central meridian
  a = DatumEqRad[Item];#equatorial radius, meters. 
  f = 1/DatumFlat[Item];#polar flattening.
  b = a*(1-f);#polar axis.
  #e = sqrt(1 -b*b/a*a);#eccentricity
  drad = pi/180;#Convert degrees to radians)
  latd = 0;#latitude in degrees
  phi = 0;#latitude (north +, south -), but uses phi in reference
  #e0 = e/sqrt(1 - e*e);#e prime in reference
  #N = a/sqrt(1-(e*sin(phi))^2);
  T = (tan(phi)^2);
  #C = (e*cos(phi)^2);
  lng = 0;#Longitude (e = +, w = -) - can't use long - reserved word
  lng0 = 0;#longitude of central meridian
  lngd = 0;#longitude in degrees
  M = 0;#M requires calculation
  x = 0;#x coordinate
  y = 0;#y coordinate
  k = 1;#local scale
  utmz = 30;#utm zone
  zcm = 0;#zone central meridian
  DigraphLetrsE = "ABCDEFGHJKLMNPQRSTUVWXYZ";
  DigraphLetrsN = "ABCDEFGHJKLMNPQRSTUV";
  #document.getElementById("EqRadBox").value = a;
  #document.getElementById("PolRadBox").value = b;
  #document.getElementById("FlatBox").value = f;
  #document.getElementById("RecipBox").value = 1/f;
  #OOZok = false;
  
  pow = function(a,b){
    return(a^b)
  }
  
  #Convert UTM Coordinates to Geographic
  
  k0 = 0.9996;#scale on central meridian
  b = a*(1-f);#polar axis.
  e = sqrt(1 - (b/a)*(b/a));#eccentricity
  e0 = e/sqrt(1 - e*e);#Called e prime in reference
  esq = (1 - (b/a)*(b/a));#e squared for use in expansions
  e0sq = e*e/(1-e*e);# e0 squared - always even powers
  x = UTMX
  # 
  y = UTMY
  
  utmz = 30
  zcm = 3 + 6*(utmz-1) - 180;#Central meridian of zone
  e1 = (1 - sqrt(1 - e*e))/(1 + sqrt(1 - e*e));#Called e1 in USGS PP 1395 also
  M0 = 0;#In case origin other than zero lat - not needed for standard UTM
  M = M0 + y/k0;#Arc length along standard meridian. 
  mu = M/(a*(1 - esq*(1/4 + esq*(3/64 + 5*esq/256))));
  phi1 = mu + e1*(3/2 - 27*e1*e1/32)*sin(2*mu) + e1*e1*(21/16 -55*e1*e1/32)*sin(4*mu);#Footprint Latitude
  phi1 = phi1 + e1*e1*e1*(sin(6*mu)*151/96 + e1*sin(8*mu)*1097/512);
  C1 = e0sq*pow(cos(phi1),2);
  T1 = pow(tan(phi1),2);
  N1 = a/sqrt(1-pow(e*sin(phi1),2));
  R1 = N1*(1-e*e)/(1-pow(e*sin(phi1),2));
  D = (x-500000)/(N1*k0);
  phi = (D*D)*(1/2 - D*D*(5 + 3*T1 + 10*C1 - 4*C1*C1 - 9*e0sq)/24);
  phi = phi + pow(D,6)*(61 + 90*T1 + 298*C1 + 45*T1*T1 -252*e0sq - 3*C1*C1)/720;
  phi = phi1 - (N1*tan(phi1)/R1)*phi;
  
  #Output Latitude
  latitude=floor(1000000*phi/drad)/1000000;
  
  #Longitude
  lng = D*(1 + D*D*((-1 -2*T1 -C1)/6 + D*D*(5 - 2*C1 + 28*T1 - 3*C1*C1 +8*e0sq + 24*T1*T1)/120))/cos(phi1);
  lngd = zcm+lng/drad;
  
  #Output Longitude
  longitude = floor(1000000*lngd)/1000000;
  
  #Altitude
  library(RCurl)
  library(RJSONIO)
  library(plyr)
  
  
  root <- "https://maps.googleapis.com/maps/api/elevation/json?locations="
  u <- paste(root,latitude,",",longitude, sep = "")
  google<- URLencode(u)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK"){
    elevation=x$results[[1]]$elevation
  }else elevation=NA
  
  return(list(latitude=latitude,longitude=longitude,elevation=elevation))
}
