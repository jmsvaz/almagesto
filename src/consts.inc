const

  //number of radians in one revolution
    RadiansPerRev = 2*Pi;
  //number of degrees in one revolution
    DegreesPerRev = 360;

  //  Julian Date of epoch J2000.0 (2000 January 1, 12h)
    J2000  = 2451545.0;
  //  Julian Date of epoch J1900.0 (1900 January 1, 12h)
    J1900  = 2415020.0;
  //  Julian Date of epoch B1950.0 (1950 January 1, 12h)
    B1950  = 2433282.42345905;
  //  Julian Date of epoch B1900.0 (1900 January 1, 12h)
    B1900  = 2415020.31352;
  //  Julian Date of epoch B1850.0 (1850 January 1, 12h)
    B1850  = 2396758.20358095;

  // Julian Date on 2000 January 1, 0h
    JulianDate2000 = 2451544.5;

  // number of Julian days in one year
    JulianDaysPerYear       = 365.25;
  // number of Julian days in one century
    JulianDaysPerCentury    = 100*JulianDaysPerYear;
  // number of Julian days in one millenium
      JulianDaysPerMillenium    = 1000*JulianDaysPerYear;
  // number of Tropical days in one year
    TropicalDaysPerYear     = 365.242198781;
  // number of Tropical days in one century
    TropicalDaysPerCentury  = 100*TropicalDaysPerYear;

  { Defining value of "1-d(TT)/d(TCG)" : IAU Resolution 3 (2006). }
  LB = 1.550519768e-8;
  // Defining value of "1-d(TDB)/d(TCB)" : IAU Resolution B1.9 (2000)
  LG = 6.969290134e-10;

  // The JulianDate of the event 1977 Jan 1 00:00:32.184 TAI at the geocenter,
  // where TT = TCG = TCB = T0 = 2443144.5003725
    T0 = 2443144.5003725;


