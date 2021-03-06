{
This file lists the UT1-UTC differences and should be updated as soon a new
IERS Bulletin D is released.

DUT1 is the difference UT1-UTC expressed with a precision of +/-0.1s,
which is broadcast with the time signals. The changes in DUT1 are forecast
by the IERS.

Bulletin D is mailed in an irregular basis.

The last bulletin can be found at ftp://hpiers.obspm.fr/iers/bul/buld/bulletind.dat

The incomplete series of Bulletin D can be found at https://datacenter.iers.org/availableVersions.php?id=17

- MinDate is the JulianDate of the first DUT1 at 1991-06-20;
- MaxDate is the JulianDate of the last modified file date;
- NDUT1 should be incremented as a new DUT1 is annnounced;
- DUT1_Array is the array of DUT1; a new row should be added as a new DUT1 is annnounced.

The current IERS Bulletin D is Number 114 released at 2012-Dec-21:
- From the  31 January 2013, 0h UTC, until further notice, the value of DUT1 = +0.2 s
- Bulletin D 115 should be issued in March 2013
}

const
  MinDate = 2448427.5; // >= 1991-06-20
  MaxDate = 2456372.5; // <  2013-03-21 (next estimated Bulletim D release date)
  NDUT1 = 83;
  DUT1_Array: array [1..NDUT1,1..2] of Single =
  // UT1-UTC (T) = A  (in seconds)
  //    T          A

   ((2448427.5,	+0.2),
    (2448490.5,	+0.1),
    (2448546.5,	+0.0),
    (2448581.5,	-0.1),
    (2448749.5,	-0.5),
    (2448868.5,	+0.3),
    (2448917.5,	+0.2),
    (2448952.5,	+0.1),
    (2449001.5,	+0.0),
    (2449113.5,	-0.3),
    (2449169.5,	+0.6),
    (2449267.5,	+0.4),
    (2449309.5,	+0.3),
    (2449351.5,	+0.2),
    (2449428.5,	+0.0),
    (2449463.5,	-0.1),
    (2449534.5,	+0.8),
    (2449575.5,	+0.7),
    (2449771.5,	+0.3),
    (2449792.5,	+0.2),
    (2449820.5,	+0.1),
    (2449865.5,	+0.0),
    (2449911.5,	-0.1),
    (2449967.5,	-0.2),
    (2450016.5,	-0.3),
    (2450051.5,	-0.4),
    (2450083.5,	+0.5),
    (2450135.5,	+0.4),
    (2450184.5,	+0.3),
    (2450219.5,	+0.2),
    (2450303.5,	+0.1),
    (2450359.5,	+0.0),
    (2450422.5,	-0.1),
    (2450485.5,	-0.2),
    (2450527.5,	-0.3),
    (2450576.5,	-0.4),
    (2450625.5,	-0.5),
    (2450630.5,	+0.5),
    (2450709.5,	+0.4),
    (2450751.5,	+0.3),
    (2450800.5,	+0.2),
    (2450863.5,	+0.1),
    (2450898.5,	+0.0),
    (2450940.5,	-0.1),
    (2451038.5,	-0.2),
    (2451143.5,	-0.3),
    (2451179.5,	+0.7),
    (2451241.5,	+0.6),
    (2451325.5,	+0.5),
    (2451465.5,	+0.4),
    (2451549.5,	+0.3),
    (2451647.5,	+0.2),
    (2451836.5,	+0.1),
    (2451969.5,	+0.0),
    (2452186.5,	-0.1),
    (2452319.5,	-0.2),
    (2452571.5,	-0.3),
    (2452732.5,	-0.4),
    (2453124.5,	-0.5),
    (2453446.5,	-0.6),
    (2453736.5,	+0.3),
    (2453852.5,	+0.2),
    (2454006.5,	+0.1),
    (2454091.5,	+0.0),
    (2454174.5,	-0.1),
    (2454265.5,	-0.2),
    (2454433.5,	-0.3),
    (2454538.5,	-0.4),
    (2454685.5,	-0.5),
    (2454790.5,	-0.6),
    (2454902.5,	+0.3),
    (2454993.5,	+0.2),
    (2455147.5,	+0.1),
    (2455266.5,	+0.0),
    (2455350.5,	-0.1),
    (2455567.5, -0.2),
    (2455693.5, -0.3),
    (2455869.5, -0.4),
    (2455966.5, -0.5),
    (2456057.5, -0.6),
    (2456109.5, +0.4),
    (2456225.5, +0.3),
    (2456323.5, +0.2));
