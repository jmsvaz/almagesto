Using this library should be as easy as this:

	SolarSystemCatalog:= TJPLEphem.Create('/path/to/data/files'); // use the JPL Ephemerides for planets and Moon
	StarCatalog:= TFK5.Create('/path/to/data/files');  // use the FK5 star catalog
	EOP:= TEOP.Create('/path/to/data/files');   // where to get Earth Orientation Parameters for high precision computations
	
	TimeServer:= TTimeServer.Create;
	TimeServer.Model:= tsmFB2001;   // use Fairhead and Bretagnon model to transform TDT to TDB or TCG to TCB
	
	Observer:= TEarth.Create(SolarSystemCatalog,EOP);  // we make the observation at the Earth
	Observer.Precision:= opRelativistic;  // use relativistic transformation for BCRS-GCRS 
	
	TimeServer.TCG:= Observer.TCG;  // get the time of observation at the local reference frame

	Observer.SetPosition(ALat,Along,AHeight,ATimeZone,ADayLightSavings);  // position of observation
	Observer.SetLocalTime(Now);  // time of observation

	for Body in SolarSystemCatalog do  // try to use an iterator...
	  begin
	    Body.TCB:= TimeServer.TCB;  // set the time of observation at the barycentric reference frame
	    View.XYZ:= Observer.Observ(Body);
	    drawBody(View.RADecR);
	  end;
