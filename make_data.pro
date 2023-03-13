pro make_data

; make a fake galaxy spectrum and images

; parameters
line_width = 50 ; in Angstroms
bulge_radius = 150 ; in pixels
disk_radius = 150 ; in pixels

; spectrum

; read in the galaxy spectrum
spectrum_galaxy = fltarr(50000)
openr, unit, 'spectrum_galaxy.dat', /get_lun
a = 0.
b = 0.
while not eof(unit) do begin 
 readf, unit, a, b 
 spectrum_galaxy(floor(a)) = b
endwhile
close, unit
free_lun, unit
 
; read in the spectral lines
line = fltarr(61)
strength = fltarr(61)
openr, unit, 'spectrum_lines.dat', /get_lun
a = 0.
b = 0.
for i = 0, 60 do begin
 readf, unit, a, b
 line(i) = a
 strength(i) = b
endfor
close, unit
free_lun, unit
 
; add the spectral lines
for i = 0, 60 do begin
 if (spectrum_galaxy(floor(line(i))) lt strength(i)) then begin
  spectrum_galaxy(floor(line(i))) = strength(i)
 endif
endfor

; now, we only want from 0.0 to 3.0 microns
spectrum_galaxy = spectrum_galaxy(0: 29999)

; smooth this to produce artificial line widths
spectrum_galaxy = smooth(spectrum_galaxy, line_width)

; normalize
spectrum_galaxy = spectrum_galaxy/total(spectrum_galaxy)

; and write this to a file
get_lun, unit
openw, unit, 'perfect_spectrum.dat'
for i = 0, 29999 do begin
 printf, unit, float(i), spectrum_galaxy(i)
endfor
close, unit
free_lun, unit
print, 'made perfect galaxy spectrum'

; image

; read in the galaxy-field image, a 1 second 1100 X 1300 pixel H image from the HST NICMOS HDF-S
image = readfits('hdfs_nicmos.fits', /silent)

; extract the bright star from this image and normalize it to unity
aperture = shift((dist(40)*2./40. lt 1. and dist(40)*2./40. ge 0.), 20., 20.)
star = aperture*image(278 - 2 - 20:278 - 2 + 19, 599 - 1 - 20:599 - 1 + 19)
star = zero(star)
star = star/total(star)

; extract the bright spiral galaxy and rebin it
spiral = image(760 - 50:760 + 49, 831 - 50:831 + 49)
spiral = zero(spiral)
spiral = congrid(spiral, 300, 300)

; mask out the brightest knots
mask = 1. - aperture
;spiral(131 - 5:170 - 5, 131 - 5:170 - 5) = mask*spiral(131 - 5:170 - 5, 131 - 5:170 - 5)
spiral(141 - 5:180 - 5, 233 - 5:272 - 5) = mask*spiral(141 - 5:180 - 5, 233 - 5:272 - 5)

; deconvolve
for n = 1, 5 do begin
 max_likelihood, spiral, star, output
 spiral = output
endfor

; cut and normalize the spiral to unity
aperture = shift((dist(300)*2./300. lt 1. and dist(300)*2./300. ge 0.), 150., 150.)
spiral = aperture*spiral
spiral = spiral/total(spiral)

; continuing to deconvolve will generate even more structure
bursts = spiral
for n = 1, 15 do begin
 max_likelihood, bursts, star, output
 bursts = output
endfor
spiral = aperture*(spiral + bursts)
spiral = spiral/total(spiral)

; make the bulge component
bulge = fltarr(300, 300)
for i = 0, 299 do begin
 for j = 0, 299 do begin
  bulge(i, j) = sqrt((i - 150.)^2+(j - 150.)^2)
 endfor
endfor
bulge=aperture*exp(-7.67*(bulge/bulge_radius)^0.25)
bulge = bulge/total(bulge)

; make the exponential disk component
disk = fltarr(300, 300)
for i = 0, 299 do begin
 for j = 0, 299 do begin
  disk(i,j) = sqrt((i - 150.)^2+(j - 150.)^2)
 endfor
endfor
disk = aperture*exp(-disk/disk_radius)
disk = disk/total(disk)

; make random bright knots
;knot = psf_gaussian(npixel = 20, fwhm = 1., /normal) ; a Gaussian
knot = fltarr(20, 20) ; a delta function
knot[10:10] = 1.
knot = max(spiral)*knot
knots = fltarr(300, 300)
for i = 0, 9 do begin
 x = 280*randomu(seed)
 y = 280*randomu(seed)
 knots(x:x + 19, y:y + 19) = randomu(seed)*knot
endfor
knots = aperture*knots
knots = knots/total(knots)

; make randomn faint knots
knot = knot/10.
for i = 0, 99 do begin
 x = 280*randomu(seed)
 y = 280*randomu(seed)
 knots(x:x + 19, y:y + 19) = randomu(seed)*knot
endfor
knots = aperture*knots
knots = knots/total(knots)

; create the disk
;disk = spiral + disk + knots
;disk = spiral + knots
disk = spiral
disk = disk/total(disk)

; combine all for display
galaxy = bulge + disk

; show these
tvscl, alog10(1000.*congrid(spiral, 256, 256) + 0.001), 0, 256
tvscl, alog10(1000.*congrid(bulge, 256, 256) + 0.001), 256, 256
;tvscl, alog10(1000.*congrid(disk, 256, 256) + 0.001), 0, 0
tvscl, alog10(1000.*congrid(knots, 256, 256) + 0.001), 0, 0
tvscl, alog10(1000.*congrid(galaxy, 256, 256) + 0.001), 256, 0

; save the galaxy images
writefits, 'perfect_bulge.fits', bulge 
writefits, 'perfect_disk.fits', disk
;; also, as JPEGs
;write_jpeg, 'spiral.jpg', alog10(10.*spiral + 0.001)
;write_jpeg, 'bulge.jpg', alog10(10.*bulge + 0.001)
;write_jpeg, 'disk.jpg', alog10(10.*disk + 0.001)
;write_jpeg, 'knots.jpg', alog10(10.*knots + 0.001)
;write_jpeg, 'galaxy.jpg', alog10(10.*knots + 0.001)
print, 'made galaxy images'

; make the background spectrum

; read in the sky spectrum. These data are from Keck NIRSPEC and are provided by Nicolas Cardiel. It covers 1.0 to 3.0 microns in 20000 datapoints.
spectrum_sky = fltarr(30000)
openr, unit, 'spectrum_sky.dat', /get_lun
a = 0.
b = 0.
for i = 0, 19999 do begin 
 readf, unit, a, b
 spectrum_sky(9999 + i)=b

 ; find places where the spectrum is negative and set these to unity
 if (spectrum_sky(i) le 0.) then begin
  spectrum_sky(i) = 1.
 endif

endfor
close, unit
free_lun, unit

;; general constants
;c = 2.99792458e8 ; meters per second
;h = 6.6260755e-34 ; Joules per second

;; calculate the sky background in units of photons per second per square meter per square arcsecond per micron. We divide by the calibration to get this in photons per micron and multiply by the wavelength. Note that the telescope pixel was 0.18 arcsec by 0.76 arcsec.
;background_sky = fltarr(30000)
;for i = 0, 29999 do begin
; background_sky(i) = 10000.*(float(i)*1e-7*1.e-10)/(1.5e17*(0.01^2)*0.1368*h*c)*spectrum_sky(i)
;endfor

;; constants
;k_b = 1.380658e-23 ; Joules per Kelvin
;sky_area = ((1./648000.)*!pi)^2 ; steradians
;t = 270 ; Kelvin

;; subtract the thermal component
;thermal_correction = fltarr(30000)
;for i = 1, 29999 do begin

; ; the thermal background
; lambda = (3.*i/30000.)*1.e-6 ; in meters
; blackbody = 2.*h*c/lambda^3.*(exp(h*c/(lambda*k_b*t))-1.)^(-1.)
; thermal_correction(i) = blackbody*sky_area/lambda/h/1.e6 ; in photons per Angstrom

;endfor
;;background_sky = background_sky - thermal_correction
;spectrum_sky = spectrum_sky - thermal_correction

; write this to a file
get_lun, unit
openw, unit, 'background_spectrum.dat'
for i = 0, 29999 do begin
 printf, unit, float(i), spectrum_sky(i) 
endfor
close, unit
free_lun, unit
print, 'made background spectrum'
print, 'done'

end
