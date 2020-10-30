# ALMAGESTO

## Independent data

- UTC (Julian date): Coordinated Universal Time, UTC = TAI + n, where n is an integer number of seconds, such that UT1 – UTC < 0.9
- DAT (seconds of time): The acumulated number of leap seconds
- DUT (seconds of time): The current value of the difference UT1 - UTC
- xp, yp (radians): Coordinates of the Celestial Intermediate Pole (CIP) with respect to the ITRS (polar motion)
- dψ, dε or dX, dY (radians): Time-dependente corrections to the precession-nutation model, determined by observations, provided by IERS (dψ, dε with respect to the IAU 1976/1980 precession/nutation model and dX, dY with respect to the IAU 2000A precession/nutation model)
- λ (radians): Observer's geodetic longitude, reckoned negatively for the western hemisphere
- φ (radians): Observer's geodetic latitude, reckoned negatively for the southern hemisphere
- h (meters): Observer's height above the reference ellipsoid

## Intermediary data

- UT1 (Julian date): Universal Time, the angle of the Earth’s rotation about the CIP axis defined by its conventional linear relation to the Earth Rotation Angle (ERA)
- TT (Julian date): Terrestrial Time, a coordinate time whose mean rate is close to the mean rate of the proper time of an observer located on the rotating geoid
- TAI (Julian date): International Atomic Time, a widely used practical realization of TT with a fixed shift from the latter due to historical reasons
- TDB (Julian date): Barycentric Dynamical Time, the time used as the independent variable in solar sustem barycentric ephemerides
- θ (radians): Earth Rotation Angle (ERA), angle measured along the intermediate equator of the Celestial Intermediate Pole (CIP) between the Terrestrial Intermediate Origin (TIO) and the Celestial Intermediate Origin (CIO), positively in the retrograde direction
- GMST (radians): Greenwich Mean Sidereal Time, Greenwich hour angle of the mean equinox defined by a conventional relationship to Earth Rotation Angle or equivalently to UT1.
- EE (radians): Equation of Equinoxes, the right ascension of the mean equinox referred to the true equator and equinox
- GAST (radians): Greenwich Apparent Sidereal Time, the hour angle of the true equinox from the Terrestrial Intermediate Origin (TIO) meridian (Greenwich or International meridian)
- LAST (radians): Local Apparent Sidereal Time
- δψB,δεB (radians): Frame bias in longitude and obliquity, offsets of the mean equator and (dynamical) mean equinox of J2000.0, provided by the current model, with respect to the GCRS
- ξ0, η0 (radians): Frame bias in rectangular coordinates, offsets of the mean equator and (dynamical) mean equinox of J2000.0, provided by the current model, with respect to the GCRS
- dα0 (radianas): Frame bias in right ascension, equinox offset at J2000.0
- ε0 (radians): Obliquity of ecliptic at epoch (currently J2000.0)
- εA (radians): Obliquity of the ecliptic at date (Mean Obliquity)
- ζA, θA, zA (radians): Equatorial precession angles (1st, 2nd and 3rd 323 Euler angles)
- ψA, ωA, χA (radians): Luni-solar precession; inclination of equator wrt J2000.0 ecliptic and planetary precession
- ∆ψ, ∆ε (radians): Nutation in longitude and nutation in obliquity angles
- X, Y (radians): Coordinates of the Celestial Intermediate Pole (CIP) with respect to the GCRS that include frame bias, precession and nutation at date t
- s (radians): The difference between the GCRS right ascension and the intermediate right ascension of the intersection of the GCRS and intermediate equators. Locates the celestial intermediate origin (CIO) on the CIP equator
- s′ (radians): The difference between the ITRS longitude and the instantaneous longitude of the intersection of the ITRS and intermediate equators. Locates the terrestrial intermediate origin (TIO) on the CIP equator

## other data

- TCG
- TCB

## data relations

- UT1 = UTC + DUT
- ∆AT = DAT, or ∆AT = ∆T - ∆TAI
- TAI = UTC + ∆AT
- TT = TAI + ∆TAI 
- GAST = GMST + EE
- LAST = GAST + λ

## canonical functions

- ∆T
- θ
- GMST
- EE
- δψB,δεB
- ξ0, η0
- dα0
- ε0
- εA
- ζA, θA, zA
- ψA, ωA, χA
- ∆ψ, ∆ε
- X, Y
- s
- s′

## other relations

- ∆TAI = 32'.184
