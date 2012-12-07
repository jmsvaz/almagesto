{
This file lists the TAI-UTC differences and should be updated as soon a new
IERS Bulletin C is released.

UTC is defined by the CCIR Recommendation 460-4 (1986). It differs from TAI
by an integral number of seconds, in such a way that UT1-UTC stays smaller
than 0.9s in absolute value. The decision to introduce a leap second in UTC
to meet this condition is the responsability of the IERS. According to the
CCIR Recommendation, first preference is given to the opportunities at the
end of December and June,and second preference to those at the end of March
and September. Since the system was introduced in 1972 only dates in June
and December have been used.

Bulletin C is mailed every six months, either to announce a time step in UTC,
or to confirm that there will be no time step at the next possible date.

The last bulletin can be found at ftp://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat

The complete series can be found at ftp://maia.usno.navy.mil/ser7/tai-utc.dat

- MinDate is the JulianDate of the first leap second introduced at 1961-01-01;
- MaxDate is the JulianDate of the date at the bulletin;
- NLeapSeconds should be incremented as a new leap second is annnounced;
- LeapSeconds is the array of leap seconds; a new row should be added as a new leap second is annnounced.

The current IERS Bulletin C is Number 44 released at 2012-08-07:
- NO leap second will be introduced at the end of December 2012.
- from 2012 July 1, 0h UTC, until further notice : UTC-TAI = -35 s
}

const
  MinDate = 2437300.5; // >= 1961-01-01
  MaxDate = 2456474.5; // <  2013-07-01
  NLeapSeconds = 39;
  LeapSeconds: array [1..NLeapSeconds,1..4] of Single =
  // TAI-UTC (T) = A + (MJD - B) x C (in seconds)
  //    T           A        B        C
  ((2437300.5,  1.4228180, 37300, 0.001296 ),
   (2437512.5,  1.3728180, 37300, 0.001296 ),
   (2437665.5,  1.8458580, 37665, 0.0011232),
   (2438334.5,  1.9458580, 37665, 0.0011232),
   (2438395.5,  3.2401300, 38761, 0.001296 ),
   (2438486.5,  3.3401300, 38761, 0.001296 ),
   (2438639.5,  3.4401300, 38761, 0.001296 ),
   (2438761.5,  3.5401300, 38761, 0.001296 ),
   (2438820.5,  3.6401300, 38761, 0.001296 ),
   (2438942.5,  3.7401300, 38761, 0.001296 ),
   (2439004.5,  3.8401300, 38761, 0.001296 ),
   (2439126.5,  4.3131700, 39126, 0.002592 ),
   (2439887.5,  4.2131700, 39126, 0.002592 ),
   (2441317.5, 10.0,       41317, 0.0      ),
   (2441499.5, 11.0,       41317, 0.0      ),
   (2441683.5, 12.0,       41317, 0.0      ),
   (2442048.5, 13.0,       41317, 0.0      ),
   (2442413.5, 14.0,       41317, 0.0      ),
   (2442778.5, 15.0,       41317, 0.0      ),
   (2443144.5, 16.0,       41317, 0.0      ),
   (2443509.5, 17.0,       41317, 0.0      ),
   (2443874.5, 18.0,       41317, 0.0      ),
   (2444239.5, 19.0,       41317, 0.0      ),
   (2444786.5, 20.0,       41317, 0.0      ),
   (2445151.5, 21.0,       41317, 0.0      ),
   (2445516.5, 22.0,       41317, 0.0      ),
   (2446247.5, 23.0,       41317, 0.0      ),
   (2447161.5, 24.0,       41317, 0.0      ),
   (2447892.5, 25.0,       41317, 0.0      ),
   (2448257.5, 26.0,       41317, 0.0      ),
   (2448804.5, 27.0,       41317, 0.0      ),
   (2449169.5, 28.0,       41317, 0.0      ),
   (2449534.5, 29.0,       41317, 0.0      ),
   (2450083.5, 30.0,       41317, 0.0      ),
   (2450630.5, 31.0,       41317, 0.0      ),
   (2451179.5, 32.0,       41317, 0.0      ),
   (2453736.5, 33.0,       41317, 0.0      ),
   (2454832.5, 34.0,       41317, 0.0      ),
   (2456109.5, 35.0,       41317, 0.0      ));
