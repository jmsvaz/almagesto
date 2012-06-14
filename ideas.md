# Ideas to Almagesto Astronomical Library #

## Usage ##

Using this library should be as easy as this:

	SolarSystemCatalog:= TVSOP.Create; // use the VSOP for planets
	StarCatalog:= TFK5.Create('/path/to/data/files');  // use the FK5 star catalog
	
	Observer:= TEarth.Create(SolarSystemCatalog);  // we make the observation at the Earth
	
	Observer.SetPosition(ALat,Along,AHeight,ATimeZone,ADayLightSavings);  // position of observation
	Observer.SetLocalTime(Now);  // time of observation

	for Body in SolarSystemCatalog do  // iterate through solar system bodies
	  begin
	    View:= Observer.Observ(Body);  // make the observation and get a view of the body at the observer coordinate system
	    drawBody(View.RADecR);   // call a function to draw the planet on the screen
	  end;

	for Star in StarCatalog do  // // iterate through stars
	  begin
	    View:= Observer.Observ(Star);  // make the observation and get a view of the body at the observer coordinate system
	    drawBody(View.RADecR);   // call a function to draw the planet on the screen
	  end;

But we can set some advanced options if we need:

	SolarSystemCatalog:= TJPLEphem.Create('/path/to/data/files'); // use the JPL Ephemerides for planets and Moon
	StarCatalog:= TFK5.Create('/path/to/data/files');  // use the FK5 star catalog
	EOP:= TEOP.Create('/path/to/data/files');   // where to get Earth Orientation Parameters for high precision computations

	TimeServer:= TFB2001.Create;  // use Fairhead and Bretagnon model to transform from TT to TDB
	
	Observer:= TEarth.Create(SolarSystemCatalog,EOP);  // we make the observation at the Earth
	Observer.Precision:= opRelativistic;  // use relativistic transformation for BCRS to GCRS 
	Observer.TimeServer:= TimeServer;  // set the time server
	
	Observer.SetPosition(ALat,Along,AHeight,ATimeZone,ADayLightSavings);  // position of observation
	Observer.SetLocalTime(Now);  // time of observation

	for Body in SolarSystemCatalog do  // iterate through solar system bodies
	  begin
	    View:= Observer.Observ(Body);  // make the observation and get a view of the body at the observer coordinate system
	    drawBody(View.RADecR);   // call a function to draw the planet on the screen
	  end;

	for Star in StarCatalog do  // // iterate through stars
	  begin
	    View:= Observer.Observ(Star);  // make the observation and get a view of the body at the observer coordinate system
	    drawBody(View.RADecR);   // call a function to draw the planet on the screen
	  end;

## Responsabilities ##

### TObserver ###

- the __observation__ transforms from the barycentric reference system (BCRS) to the local reference system. 
- the transformation from the BCRS to the local reference system can be classic (Galilean), including aberration of light, or relativistic, with precision of mas or µas, including gravitational light deflection.
- it's an abstract class and the concrete class implements specific algorithms.
- the constructor may be an TEarth class or a satellite orbiting the sun, for example.
- all the time and position transformation is done in the concrete class (TEarth class: UT1, UTC, TAI, TT, Precession, Nutation; TMoon: Librations)

### TTimeServer ###

- transforms the time scale from BCRS to GCRS. 
- serve TT, TDB, TCG and TCB.
- it's an abstract class and the concrete class implements the periodic terms model.
- models: Fairhead and Bretagnon, Harada and Fukushima, EPM etc

### TEOP ###

- this class gets the Earth  Orientation Parameters (EOP): celestial pole offsets (Δψ,Δε) or (ΔX,ΔY), polar motion (x,y), and Universal Time (ΔUT = UT1-UTC)
- historical through [http://data.iers.org/products/214/14443/orig/eopc04_08_IAU2000.62-now]
- future through IERS Bulletim A
