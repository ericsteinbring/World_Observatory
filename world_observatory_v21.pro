;--------------------------------------
; world_observatory_v21
; Eric Steinbring
; V2.0 2 June 2005
; V2.1 10 December 2005
; See world_observatory_v21.doc for more information.

;--------------------------------------
pro show_information

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

; show information
wset, picture_def

; print author information to the screen
;erase, 255
blank = fltarr(256, 384) + 255.
tv, blank, 0, 0
xyouts, 20, 192 + 60, 'World Observatory', charsize = 2.5, color = 0, /device
xyouts, 20, 192 + 45, 'Eric Steinbring', charsize = 1.25, color = 0, /device
xyouts, 20, 192 + 30, 'Herzberg Institute for Astrophysics', charsize = 1.25, color = 0, /device
xyouts, 20, 192 + 15, 'National Research Council Canada', charsize = 1.25, color = 0, /device
xyouts, 20, 192, 'Victoria, BC, V9E 2E7', charsize = 1.25, color = 0, /device
xyouts, 20, 192 - 15, 'Eric.Steinbring@nrc-cnrc.gc.ca', charsize = 1.25, color = 0, /device
xyouts, 20, 192 - 30, 'members.shaw.ca/ericsteinbring', charsize = 1.25, color = 0, /device
xyouts, 20, 192 - 45, 'Version 2.1: 10 December 2005', charsize = 1.25, color = 0, /device

end

;--------------------------------------
pro set_defaults, parameter

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def, $
                 deceleration_def, $
                 magnitude_def, bulge_radius_def, bulge_to_total_given_def, inclination_def, redshift_def, fixed_def, galaxies_def, metric_def

common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   deceleration_min, $
                   magnitude_min, bulge_radius_min, bulge_to_total_given_min, inclination_min, redshift_min, galaxies_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max, $
                   deceleration_max, $
                   magnitude_max, bulge_radius_max, bulge_to_total_given_max, inclination_max, redshift_max, galaxies_max

common inputs, field, perfect_bulge, perfect_disk, perfect_spectrum, background_spectrum

; set the global defaults
if parameter eq 0 then begin

 ; site
 site_def = 0 ; choose a mid-latitude site or a polar site
 polar_cost_def = 1.5 ; how much more expensive it is to build at a polar site compared to a mid-latitude site
 polar_background_def = 20. ; how much fainter the K-band polar sky is compared to a mid-latitude site
 polar_natural_def = 2. ; how much better the natural seeing is at a polar site compared to a mid-latitude site
 polar_ground_def = 1. ; how much better the ground seeing is at a polar site compared to a mid-latitude site
 altitude_def = 4200. ; altitude of the site in meters
 road_length_def = 0. ; length of required road in kilometers

 ; telescope
 telescope_def = 4 ; choice of telescope
 diameter_def = 10. ; diameter of the telescope in meters
 focal_ratio_def = 1.8 ; focal ratio of the telescope
 pier_height_def = 10. ; height of the telescope pier in meters
 wavelength_def = 2.2 ; wavelength of observation in microns
 strehl_def = 0.37 ; Strehl ratio at the given wavelength

 ; costs
 road_cost_def = 1.e4 ; cost of road per kilometer in dollars
; enclosure_cost_def = 10. ; cost of enclosure per cubic meter in dollars
 enclosure_cost_def = 5.e3 ; cost of enclosure per square meter in dollars
 telescope_cost_def = 3.e5 ; cost of telescope per square meter of aperture in dollars 
 actuator_cost_def = 5.e4 ; cost per adaptive optics actuator in dollars

 ; cosmology
 deceleration_def = 0.5 ; deceleration parameter

 ; galaxy
 magnitude_def = -26. ; absolute magnitude of the galaxy
 bulge_radius_def = 0.5 ; bulge radius in kiloparsecs
 bulge_to_total_given_def = 0.5 ; bulge-to-total ratio of the galaxy
 inclination_def = 70. ; inclination of the galaxy in degrees
 redshift_def = 0.1 ; redshift of the galaxy

 ; survey
 fixed_def = 0 ; choose which galaxy parameter is fixed
 galaxies_def = 10 ; number of galaxies in survey
 metric_def = 0 ; choose which metric will be measured

 ; set the defaults
 site = site_def 
 polar_cost = polar_cost_def
 polar_background = polar_background_def
 polar_natural = polar_natural_def
 polar_ground = polar_ground_def
 altitude = altitude_def
 road_length = road_length_def
 telescope = telescope_def
 diameter = diameter_def
 focal_ratio = focal_ratio_def
 pier_height = pier_height_def
 wavelength = wavelength_def
 strehl = strehl_def
 road_cost = road_cost_def
 enclosure_cost = enclosure_cost_def
 telescope_cost = telescope_cost_def
 actuator_cost = actuator_cost_def
 deceleration = deceleration_def
 magnitude = magnitude_def
 bulge_radius = bulge_radius_def
 bulge_to_total_given = bulge_to_total_given_def
 inclination = inclination_def
 redshift = redshift_def
 fixed = fixed_def
 galaxies = galaxies_def
 metric = metric_def

endif

; reset the telescope defaults
if parameter eq 1 then begin

 ; Douglas Mawson Telescope
 if telescope eq 0 then begin
  site_def = 1 ; polar site
  altitude_def = 3300. ; meters
  road_length_def = 1100. ; kilometers
  diameter_def = 2.0 ; meters
  focal_ratio_def = 2. ; dimensionless
  pier_height_def = 10. ; meters
 endif

 ; Canada-France-Hawaii Telescope (CFHT)
 if telescope eq 1 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 3.6 ; meters
  focal_ratio_def = 3.8 ; dimensionless
  pier_height_def = 22. ; meters
 endif

 ; Gemini North Telescope
 if telescope eq 2 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 8.1 ; meters
  focal_ratio_def = 1.8 ; dimensionless
  pier_height_def = 21. ; meters
 endif

 ; Gemini South Telescope
 if telescope eq 3 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 2700. ; meters
  road_length_def = 50. ; kilometers
  diameter_def = 8.1 ; meters
  focal_ratio_def = 1.8 ; dimensionless
  pier_height_def = 21. ; meters
 endif

 ; W. M. Keck Observatory
 if telescope eq 4 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 10. ; meters
  focal_ratio_def = 1.8 ; dimensionless
  pier_height_def = 10. ; meters
 endif

 ; Very Large Optical Telescope (VLOT)
 if telescope eq 5 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 20. ; meters
  focal_ratio_def = 1. ; dimensionless
  pier_height_def = 10. ; meters
 endif

 ; California Extremely Large Telescope (CELT)
 if telescope eq 6 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4100. ; meters
  road_length_def = 2. ; kilometers
  diameter_def = 30. ; meters
  focal_ratio_def = 1. ; dimensionless
  pier_height_def = 20. ; meters
 endif

 ; Euro50
 if telescope eq 7 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 2400. ; meters
  road_length_def = 2. ; kilometers
  diameter_def = 50. ; meters
  focal_ratio_def = 1. ; dimensionless
  pier_height_def = 10. ; meters
 endif

 ; Overwhelmingly Large Telescope (OWL)
 if telescope eq 8 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 2400. ; meters
  road_length_def = 2. ; kilometers
  diameter_def = 100. ; diameter of the telescope in meters
  focal_ratio_def = 1. ; focal ratio of the telescope
  pier_height_def = 0. ; height of the telescope pier in meters
 endif

; ; set the defaults, this can be shut off in order to allow switch of default without change of observatory
; site = site_def
; altitude = altitude_def
; road_length = road_length_def
; diameter = diameter_def
; focal_ratio = focal_ratio_def
; pier_height = pier_height_def

endif

; guardrails

; site
polar_cost_min = 1. ; how much more expensive it is to build at a polar site compared to a mid-latitude site
polar_cost_max = 10.
polar_background_min = 1. ; how much fainter the K-band background is at a polar site compared to a mid-latitude site
polar_background_max = 100.
polar_natural_min = 1. ; how much better the natural seeing is at a polar site compared to a mid-latitude site
polar_natural_max = 10.
polar_ground_min = 1. ; how much better the ground seeing is at a polar site compared to a mid-latitude site
polar_ground_max = 10.
altitude_min = 1000. ; altitude of the site in meters
altitude_max = 6000.
road_length_min = 0. ; length of required road in kilometers
road_length_max = 1500.

; telescope
diameter_min = 2. ; diameter of the telescope in meters
diameter_max = 100.
focal_ratio_min = 1. ; focal ratio of the telescope
focal_ratio_max = 4.
pier_height_min = 0. ; height of the telescope pier in meters
pier_height_max = 50.
wavelength_min = 1.1 ; wavelength of observation in microns
wavelength_max = 2.7
strehl_min = 0.1 ; Strehl ratio at the given wavelength
strehl_max = 0.9

; costs
road_cost_min = 1.e3 ; cost of road per kilometer in dollars
road_cost_max = 1.e5
;enclosure_cost_min = 1. ; cost of enclosure per cubic meter in dollars
;enclosure_cost_max = 1.e5
enclosure_cost_min = 1.e3 ; cost of enclosure per square meter in dollars
enclosure_cost_max = 1.e5
telescope_cost_min = 1.e5 ; cost of telescope per square meter of aperture in dollars
telescope_cost_max = 1.e6
actuator_cost_min = 1.e3 ; cost per adaptive optics actuator in dollars
actuator_cost_max = 1.e6

; cosmology
deceleration_min = 0.3 ; deceleration parameter
deceleration_max = 0.7

; galaxy
magnitude_min = -26. ; absolute magnitude of the galaxy
magnitude_max = -14.
bulge_radius_min = 0.1 ; bulge radius in kiloparsecs
bulge_radius_max = 2.5
bulge_to_total_given_min = 0. ; bulge-to-total ratio of the galaxy
bulge_to_total_given_max = 1.
inclination_max = 90. ; inclination of the galaxy in degrees
inclination_min = 0.
redshift_min = 0.1 ; redshift of the galaxy
redshift_max = 10.

; survey
galaxies_min = 1 ; number of galaxies in survey
galaxies_max = 100

; field-of-view of images
field = 0.7 ; arcseconds

; read in the perfect images and delete bad pixels
perfect_bulge = readfits('perfect_bulge.fits', /silent)
perfect_bulge = zero(perfect_bulge)
perfect_disk = readfits('perfect_disk.fits', /silent)
perfect_disk = zero(perfect_disk)

; read in the perfect spectrum, select a window in it, and rebin this to lower spectral resolution
perfect_spectrum = fltarr(30000)
openr, unit, 'perfect_spectrum.dat', /get_lun
a = 0.
b = 0.
for i = 0, 29999 do begin
 readf, unit, a, b
 perfect_spectrum(i) = b
endfor
close, unit
free_lun, unit
perfect_spectrum = perfect_spectrum[0:29999]
perfect_spectrum = congrid(perfect_spectrum, 3000) ; in nanometers

; read in the background spectrum
background_spectrum = fltarr(30000)
openr, unit, 'background_spectrum.dat', /get_lun
a = 0.
b = 0.
for i = 0, 29999 do begin
 readf, unit, a, b
 background_spectrum(i) = b
endfor
close, unit
free_lun, unit
background_spectrum = background_spectrum[0:29999]
background_spectrum = congrid(background_spectrum, 3000) ; in nanometers

end

;--------------------------------------
pro build_observatory, observatory, site, altitude, altitude_max, diameter, focal_ratio, pier_height

; build a picture of the observatory
observatory = fltarr(256, 512)

; set the ground level
ground = floor((300. - 150.)*altitude/altitude_max) + 150 ; scale height of mountain in pixels, leave room for the text display at 150 pixels high
if site eq 0 then observatory[0:255, 0:ground] = 0.67 ; mid-latitude: build a grey scaled block to represent the height of the mountain
if site gt 0 then observatory[0:255, 0:ground] = 0.95 ; polar: build an off-white scaled block to represent the height of the mountain

; set some observatory sizes
scale = 1. ; in pixels per meter
base_width = 2.*focal_ratio*diameter ; in meters
base_radius = base_width/2. ; in meters
base_height = pier_height ; in meters
dome_radius = base_radius ; in meters
shutter_width = diameter ; in meters
base_width = floor(scale*base_width) ; in pixels
base_radius = floor(scale*base_radius) ; in pixels
base_height = floor(scale*base_height) ; in pixels
dome_radius = floor(scale*dome_radius) ; in pixels
shutter_width = scale*diameter ; in pixels

; build the observatory base
if base_radius lt 3 then base_radius = 3
if base_radius gt 128 then base_radius = 128
if base_height lt 2 then base_height = 2
if base_height gt 128 then base_height = 128
observatory[128 - base_radius + 1:128 + base_radius - 1, ground:ground + base_height - 1] = 1. ; build a white block to represent the base of the observatory

; build the observatory dome
if dome_radius lt 3 then dome_radius = 3
if dome_radius gt 256 - 64 then dome_radius = 256 - 64  ; set it a bit smaller than half the image size
dome = shift((dist(2.*dome_radius)*2./float(2.*dome_radius) lt 1. and dist(2.*dome_radius)*2./float(2.*dome_radius) ge 0.), float(dome_radius), float(dome_radius)) ; make a white disk the radius of the dome
dome = dome[0:floor(2.*dome_radius) - 1, dome_radius:floor(2.*dome_radius) - 1] ; cut off the bottom half of the disk
shutter_radius = floor(shutter_width/2.)
dome[dome_radius - shutter_radius:dome_radius + shutter_radius - 1, *] = 0. ; cut out the shutter
if dome_radius lt 128 then begin ; if it will fit within the width of the image
 observatory[128 - dome_radius:128 + dome_radius - 1, ground + base_height:ground + base_height + dome_radius - 1] = dome
endif else begin ; otherwise just trim it to fit
 dome = dome[dome_radius - 128:dome_radius + 128 - 1, *]
 observatory[*, ground: ground + base_height - 1] = 1.
 observatory[*, ground + base_height:ground + base_height + dome_radius - 1] = dome
endelse

; build the telescope aperture
telescope = shift((dist(2.*shutter_radius)*2./float(2.*shutter_radius) lt 1. and dist(2.*shutter_radius)*2./float(2.*shutter_radius) ge 0.2), float(shutter_radius), float(shutter_radius)) - 0.1 ; make a grey disk the diameter of the shutter
telescope = zero(telescope)
observatory[128 - shutter_radius:128 + shutter_radius - 1, ground + base_height:ground + base_height + 2.*shutter_radius - 1] = telescope

; add a shadow
observatory[128 + 0.5*shutter_radius:*, ground:ground + base_height - 1] = zero(observatory[128 + 0.5*shutter_radius:*, ground:ground + base_height - 1] - 0.3)
observatory[128 + shutter_radius:*, ground + base_height:ground + base_height + dome_radius - 1] = zero(observatory[128 + shutter_radius:*, ground + base_height:ground + base_height + dome_radius - 1] - 0.3)

end

;--------------------------------------
pro calculate_diffraction, diffraction, diameter, wavelength

; the diffraction limited FWHM of the telescope at a given wavelength
diffraction = 206265.*(wavelength*1.e-6/diameter) ; in arcseconds, wavelength converted to meters from microns

end

;--------------------------------------
pro calculate_seeing, seeing_site, seeing_total, site, polar_natural, polar_ground, altitude, pier_height, diameter, wavelength

; the estimated mean FWHM of the seeing disk from Rene Racine's model
seeing_natural = 0.65 ; characteristic natural seeing in arcseconds
if site gt 0 then seeing_natural = seeing_natural/polar_natural ; improvement for polar site
height_natural = 7500. ; characteristic height of natural turbulence in meters
seeing_ground = 1.1 ; characteristic ground seeing in arcseconds
if site gt 0 then seeing_ground = seeing_ground/polar_ground ; improvement for polar site
height_ground = 12. ; characteristic height of ground turbulence in meters 
seeing_site = (0.5/wavelength)^(6./5.)*((seeing_natural^(5./3.) + seeing_ground^(5./3.)*exp(-5.*0./(3.*height_ground)))*exp(-altitude/height_natural))^(3./5.) ; in arcseconds, assuming telescope on ground, wavelength in microns
seeing_total = (0.5/wavelength)^(6./5.)*((seeing_natural^(5./3.) + seeing_ground^(5./3.)*exp(-5.*(pier_height + diameter/2.)/(3.*height_ground)))*exp(-altitude/height_natural))^(3./5.) ; in arcseconds, assuming telescope on a pier, wavelength in microns 

end

;--------------------------------------
pro normalize_spectrum, normalized_spectrum, diameter, deceleration, magnitude, redshift, perfect_spectrum

; constants
c = 2.99792458e8 ; meters per second
H_0 = 70. ; kilometers per second per megaparsec

; calculate the relative B-band magnitude for the given redshift
relative_magnitude = magnitude + 5.*alog10(c/H_0) - 10.*alog10(deceleration) + 5.*alog10(deceleration*redshift - (1. - deceleration)*(sqrt(1. + 2.*deceleration*redshift) - 1.)) ; from Carroll and Ostlie (1994)

; parameter
zeropoint = 25. ; the instrumental zeropoint, set to provide a reasonable K-band signal-to-noise for a 1 hour exposure on a M_B = -21 spiral galaxy with Keck AO, so this is a fudge factor

; calculate the flux
flux = 10.^((zeropoint - relative_magnitude)/2.5) ; in photons for fixed aperture, which we will assume is a square meter

; normalize the spectrum using this B-band flux
normalized_spectrum = total(flux*perfect_spectrum[400:475])*perfect_spectrum ; in photons per square meter per nanometer

; now redshift it
blue = 0
red = floor(3000./(1. + redshift)) - 1
normalized_spectrum = congrid(normalized_spectrum(blue:red), 3000) ; in photons per square meter per nanometer

; and renormalize by telescope area
normalized_spectrum = (diameter^2./1.^2.)*normalized_spectrum ; in photons per nanometer

end

;--------------------------------------
pro calculate_signal, signal, wavelength, normalized_spectrum

; calculate the signal
red = floor(0.9*wavelength*1000.) ; in nanometers
blue = floor(1.1*wavelength*1000.) ; in nanometers
signal = total(normalized_spectrum[red:blue]) ; in photons

end

;--------------------------------------
pro calculate_background, site_spectrum, background, site, diameter, wavelength, polar_background, background_spectrum

;; constants
;c = 2.99792458e8 ; meters per second
;h = 6.6260755e-34 ; Joules per second
;k_b = 1.380658e-23 ; Joules per Kelvin
;sky_area = ((1./648000.)*!pi)^2 ; steradians
;t_warm = 270 ; Kelvin
;t_cold = 210 ; Kelvin

;; calculate a thermal correction
;thermal_correction = fltarr(3000)
;for i = 1, 2999 do begin

; ; the thermal background
; lambda=(3.*i/3000.)*1.e-6 ; in meters
; blackbody_warm = 2.*h*c/lambda^3.*(exp(h*c/(lambda*k_b*t_warm))-1.)^(-1.)
; blackbody_cold = 2.*h*c/lambda^3.*(exp(h*c/(lambda*k_b*t_cold))-1.)^(-1.)
; thermal_correction(i) = (blackbody_warm - blackbody_cold)*sky_area/lambda/h/1.e6 ; in photons per nanometer
 
;endfor
;if site eq 0 then begin 
; site_spectrum = site_spectrum ; a mid-latitude site
;endif else begin
; site_spectrum = background_spectrum - thermal_correction ; a polar site
;endelse

; calculate the background for the site
red = floor(0.9*wavelength*1000.) ; in nanometers
blue = floor(1.1*wavelength*1000.) ; in nanometers
if site eq 0 then begin ; a mid-latitude site
 site_spectrum = background_spectrum ; in photons per square arcsecond per nanometer per square meter
endif else begin ; a polar site
 site_spectrum = background_spectrum/polar_background ; in photons per square arcsecond per nanometer per square meter
endelse
background = total(site_spectrum[red:blue]) ; in photons per square arcsecond per square meter

; corrected for aperture
site_spectrum = (diameter^2./1.^2.)*site_spectrum ; in photons per square arcsecond per nanometer
background = (diameter^2./1.^2.)*background ; in photons per square arcsecond

end

;--------------------------------------
pro make_images, bulge, galaxy, diffraction, deceleration, signal, field, perfect_bulge, perfect_disk, bulge_radius, bulge_to_total_given, inclination, redshift

; constants
c = 2.99792458e8 ; meters per second
H_0 = 70. ; kilometers per second per megaparsec

; calculate the scale of the perfect images
disk_diameter = 5. ; proper linear diameter of the galaxy in kiloparsecs
theta_disk = 206369.*(H_0*disk_diameter/c)*deceleration^2.*(1. + redshift)^2./(deceleration*redshift - (1. - deceleration)*(sqrt(1. + 2.*deceleration*redshift) - 1.)) ; in arcseconds
theta_bulge = 206369.*(H_0*2.*bulge_radius/c)*deceleration^2.*(1. + redshift)^2./(deceleration*redshift - (1. - deceleration)*(sqrt(1. + 2.*deceleration*redshift) - 1.)) ; in arcseconds
if theta_disk gt 3. then theta_disk = 3. ; restrict the images to be no bigger than 3 arcseconds
scale = float(round(theta_disk/diffraction)) ; size of the images in pixels
if scale le 2. then scale = 2. ; it cannot be smaller than two pixels square
center = scale/2. ; find the middle
if center mod 2. eq 0. or center mod 2. eq 1. then scale = scale + 1. ; center the image on a pixel

; rescale the perfect bulge to the given bulge radius
factor = theta_bulge/theta_disk ; how much smaller the bulge is than the disk
half_scale = floor(factor*center)
if half_scale lt 1 then half_scale = 1 ; the bulge cannot be smaller than two pixels square
bulge_tmp = congrid(perfect_bulge, 2*half_scale, 2*half_scale)
;bulge = replicate(min(bulge_tmp), scale, scale)
bulge = fltarr(scale, scale)
bulge(floor(center) - half_scale:floor(center) + half_scale - 1, floor(center) - half_scale:floor(center) + half_scale - 1) = bulge_tmp
bulge = bulge/total(bulge)

; incline the perfect disk
factor = cos(inclination*!pi/180.) ; find out how compressed the image is in the y-direction
half_scale = floor(factor*center)
if half_scale lt 1 then half_scale = 1 ; the projected vertical extent of the inclined disk cannot be less than one pixel
disk_tmp = congrid(perfect_disk, scale, 2*half_scale)
disk = replicate(min(disk_tmp), scale, scale)
disk(0:scale - 1, floor(center) - half_scale:floor(center) + half_scale - 1) = disk_tmp
disk = disk/total(disk)

; cut out the field-of-view of the telescope
center_image = scale/2. ; center of the rescaled perfect images
scale = float(round(field/diffraction)) ; size of the telescope image in pixels
if scale le 2. then scale = 2. ; it cannot be smaller than two pixels square
center = scale/2.
if center mod 2. eq 0. or center mod 2. eq 1. then scale = scale + 1. ; center the image on a pixel
half_scale = scale/2.
bulge = bulge[center_image - half_scale:center_image + half_scale - 1, center_image - half_scale:center_image + half_scale - 1] 
disk = disk[center_image - half_scale:center_image + half_scale - 1, center_image - half_scale:center_image + half_scale - 1] 

; generate the galaxy
galaxy = bulge_to_total_given*bulge + (1. - bulge_to_total_given)*disk
galaxy = galaxy/total(galaxy)

; and multiply by the signal
bulge = bulge_to_total_given*signal*bulge
galaxy = signal*galaxy

end

;--------------------------------------
pro degrade_images, image_out, image_ao_out, field, diffraction, seeing_total, galaxy, background, strehl

; generate the AO PSF
scale = float(round(seeing_total/diffraction))
if scale lt 2. then scale = 2.
center = scale/2.
if center mod 2. eq 0. or center mod 2. eq 1. then scale = scale + 1. ; center the PSF on a pixel
spike = psf_gaussian(npixel = 3.*scale, fwhm = 2., /double, /normalize) ; fixed at diffraction limited
halo = psf_gaussian(npixel = 3.*scale, fwhm = scale, /double, /normalize)
kernel = strehl*spike + (1. - strehl)*halo
kernel = kernel/total(kernel)

; generate the AO image
image_ao_out = convolve(galaxy, kernel)

; generate the non-AO PSF
kernel = halo
kernel = kernel/total(kernel)

; generate the non-AO image
image_out = convolve(galaxy, kernel)
normalization = total(image_out)
image_out = congrid(image_out, field/(seeing_total/2.), field/(seeing_total/2.)) ; rebinned to the seeing-optimized pixel scale
image_out = normalization*image_out/total(image_out)

; add noise to the images
scale = float(round(field/diffraction))
if scale lt 2. then scale = 2.
center = scale/2.
if center mod 2. eq 0. or center mod 2. eq 1. then scale = scale + 1. ; center the image on a pixel
noise = sqrt(field^2.*background/(scale)^2.)*randomn(seed, scale, scale) ; in photons per pixel at diffraction
image_ao_out = image_ao_out + noise
image_out = image_out + congrid(noise, field/(seeing_total/2.), field/(seeing_total/2.)) ; rebinned to the seeing-optimized pixel scale

end

;--------------------------------------
pro make_spectrum, spectrum_out, wavelength, normalized_spectrum, site_spectrum

; generate the spectrum
spectrum_out = normalized_spectrum
cutoff = floor(0.9*wavelength*1000.) ; in nanometers, the blueward edge of the filter
spectrum_out[0:cutoff] = smooth(spectrum_out[0:cutoff], 100) ; spectrum is unresolved shortward of the cutoff
spectrum_out[cutoff + 1:2999] = smooth(spectrum_out[cutoff + 1:2999], 2) ; wavelengths greater than the cutoff are assumed to be equally well resolved with AO

; and add noise
spectrum_out = spectrum_out + sqrt(site_spectrum)

end

;--------------------------------------
pro measure_bulge, bulge_to_total, bulge_to_total_error, image_ao_out, bulge, galaxy, bulge_to_total_given

; measure the bulge-to-total ratio assuming perfect fitting of the disk
bulge_to_total = total(image_ao_out - (galaxy - bulge))/total(image_ao_out)

; and return the error
bulge_to_total_error = abs(bulge_to_total - bulge_to_total_given)

; set limits
; bulge-to-total ratio
;if bulge_to_total lt 0.000001 then bulge_to_total = 0.000001
if bulge_to_total lt 0.001 then bulge_to_total = 0.001
if bulge_to_total gt 1. then bulge_to_total = 1.
; error in bulge-to-total ratio
;if bulge_to_total_error lt 0.000001 then bulge_to_total_error = 0.000001
if bulge_to_total_error lt 0.001 then bulge_to_total_error = 0.001
if bulge_to_total_error gt 1. then bulge_to_total_error = 1.

end

;--------------------------------------
pro measure_structure, structure, image_ao_out, image_ao_out_def, galaxy, bulge

; rebin the images to be the same
s = size(image_ao_out)
s_def = size(image_ao_out_def)
size = max([s(2), s_def(2)])
image_ao_out_tmp = congrid(image_ao_out, size, size)
image_ao_out_def_tmp = congrid(image_ao_out_def, size, size)

; and normalize
image_ao_out_tmp = zero(abs(image_ao_out_tmp))
image_ao_out_def_tmp = zero(abs(image_ao_out_def_tmp))
image_ao_out_tmp = image_ao_out_tmp/total(image_ao_out_tmp)
image_ao_out_def_tmp = image_ao_out_def_tmp/total(image_ao_out_def_tmp)
galaxy_tmp = galaxy/total(galaxy)
;bulge_tmp = bulge/total(bulge)

; measure the fidelity in recovering small structure
;blobs = sqrt(mean(image_ao_out_tmp^2.)) ; fluctuations in the image
;blobs_def = sqrt(mean(image_ao_out_def_tmp^2.)) ; fluctuations in the default image
blobs = max(image_ao_out_tmp) ; peak fluctuation in the image
blobs_def = max(image_ao_out_def_tmp) ; peak fluctuation in the default image
noise = sqrt(mean((image_ao_out_tmp - galaxy_tmp)^2.)) ; just the noise in the image
noise_def = sqrt(mean((image_ao_out_def_tmp - galaxy_tmp)^2.)) ; just the noise in the default image
structure = (blobs/noise)/(blobs_def/noise_def)

end

;--------------------------------------
pro calculate_costs, cost_site, cost_telescope, cost_total, road_length, diameter, focal_ratio, pier_height, wavelength, seeing_total, road_cost, enclosure_cost, telescope_cost, actuator_cost, strehl

;; enclosure volume assumes a hemispherical dome with a cylindrical base
;enclosure_radius = focal_ratio*diameter ; in meters
;enclosure_base = !pi*enclosure_radius^2.*pier_height ; in cubic meters
;enclosure_dome = (1./2.)*(4./3.)*!pi*enclosure_radius^3. ; in cubic meters

; enclosure surface area assumes a hemispherical dome with a cylindrical base
enclosure_radius = focal_ratio*diameter ; in meters
enclosure_base = 2.*!pi*enclosure_radius*pier_height ; in square meters
enclosure_dome = (1./2.)*4.*!pi*enclosure_radius^2. ; in square meters

; collecting area of primary
telescope_area = !pi*diameter^2. ; in square meters

; cost for telescope on the ground
cost_site = road_cost*road_length + enclosure_cost*enclosure_dome + telescope_cost*telescope_area ; in dollars

; cost with telescope on a pier
cost_telescope = cost_site + enclosure_cost*enclosure_base ; in dollars

; required number of adaptive optics actuators
fried_parameter = 206265.*(wavelength*1.e-6)/seeing_total ; in meters at the observed wavelength in microns
actuators = !pi*(0.5*diameter/fried_parameter)^2. ; to provide diffraction-limited performance with Strehl ratio of about 0.37
actuators = 4.*actuators ; fudge factor
actuators = actuators/(-1.*alog(strehl))^(6./5.) ; how many more actuators are needed to increase the Strehl ratio further

; total cost includes road, enclosure, telescope, and adaptive optics
cost_total = cost_telescope + actuator_cost*actuators ; in dollars

end

;--------------------------------------
pro show_results_def

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def, $
                 deceleration_def, $
                 magnitude_def, bulge_radius_def, bulge_to_total_given_def, inclination_def, redshift_def, fixed_def, galaxies_def, metric_def

common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   deceleration_min, $
                   magnitude_min, bulge_radius_min, bulge_to_total_given_min, inclination_min, redshift_min, galaxies_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max, $
                   deceleration_max, $
                   magnitude_max, bulge_radius_max, bulge_to_total_given_max, inclination_max, redshift_max, galaxies_max

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

common inputs, field, perfect_bulge, perfect_disk, perfect_spectrum, background_spectrum

common pictures, image_ao_out, image_ao_out_def

common outputs, cost_total, cost_total_def, snr, snr_def, average_snr, average_snr_def

; build pictures of the default observatory
build_observatory, observatory_def, site_def, altitude_def, altitude_max, diameter_def, focal_ratio_def, pier_height_def

; calculate the diffraction limit for the default observatory
calculate_diffraction, diffraction_def, diameter_def, wavelength

; normalize the galaxy spectrum for the default observatory
normalize_spectrum, normalized_spectrum_def, diameter_def, deceleration, magnitude, redshift, perfect_spectrum

; calculate the signal for the default observatory
calculate_signal, signal_def, wavelength, normalized_spectrum_def

; calculate the background for the default observatory
calculate_background, site_spectrum_def, background_def, site_def, diameter_def, wavelength, polar_background, background_spectrum

; make a perfect images for the default observatory
make_images, bulge_def, galaxy_def, diffraction_def, deceleration, signal_def, field, perfect_bulge, perfect_disk, bulge_radius, bulge_to_total_given, inclination, redshift

; calculate the seeing for the default observatory
calculate_seeing, seeing_site_def, seeing_total_def, site_def, polar_natural, polar_ground, altitude_def, diameter_def, pier_height_def, wavelength

; degrade the perfect image for the default observatory
degrade_images, image_out_def, image_ao_out_def, field, diffraction_def, seeing_total_def, galaxy_def, background_def, strehl

; make the spectrum for the default observatory
make_spectrum, spectrum_out_def, wavelength, normalized_spectrum_def, site_spectrum_def

; measure the bulge-to-total ratio for the default observatory
measure_bulge, bulge_to_total_def, bulge_to_total_error_def, image_ao_out_def, bulge_def, galaxy_def, bulge_to_total_given

; calculate the costs for the default observatory
calculate_costs, cost_site_def, cost_telescope_def, cost_total_def, road_length_def, diameter_def, focal_ratio_def, pier_height_def, wavelength, seeing_total_def, road_cost, enclosure_cost, telescope_cost, actuator_cost, strehl

; calculate the signal-to-noise ratio for the default observatory
if metric eq 0 then begin ; this needs to be metric, not metric_def
  snr_def = 1.
endif else begin
 ; use the bulge-to-total ratio instead
 snr_def = bulge_to_total_given/bulge_to_total_error_def
endelse

; display the results

; show the pictures
wset, picture_def
tvscl, observatory_def, 0, 0

; show some further information with the pictures
; telescope
tmp = ['DMT on Dome C', 'CFHT on Mauna Kea', 'Gemini on Mauna Kea', 'Gemini on Cerro Pachon', 'Keck on Mauna Kea', 'VLOT on Mauna Kea', 'CELT on Mauna Kea', 'Euro50 on La Palma', 'OWL on La Palma']
xyouts, 10, 150, tmp(telescope), color = 0, /device
; altitude
xyouts, 10, 135, 'Altitude = ' + strmid(strtrim(string(altitude_def), 1), 0, 6) + ' m', color = 0, /device
; wavelength
xyouts, 10, 120, 'Wavelength = ' + strmid(strtrim(string(wavelength), 1), 0, 3) + ' microns', color = 0, /device
; seeing
xyouts, 10, 105, 'Seeing = ' + strmid(strtrim(string(seeing_total_def), 1), 0, 4) + ' arcsec', color = 0, /device
; road length
xyouts, 10, 90, 'Road length = ' + strmid(strtrim(string(road_length_def), 1), 0, 6) + ' km', color = 0, /device
; telescope
xyouts, 10, 75, 'Telescope', color = 0, /device
; diameter
xyouts, 10, 60, 'Diameter = ' + strmid(strtrim(string(diameter_def), 1), 0, 5) + ' m', color = 0, /device
; focal ratio
xyouts, 10, 45, 'Focal ratio = ' + strmid(strtrim(string(focal_ratio_def), 1), 0, 3), color = 0, /device
; pier height
xyouts, 10, 30, 'Pier height = ' + strmid(strtrim(string(pier_height_def), 1), 0, 4) + ' m', color = 0, /device
; cost
if round(cost_total_def/1.e6) lt 1000 then begin
 xyouts, 10, 15, 'Total cost = $ ' + strmid(strtrim(string(round(cost_total_def/1.e6)), 1), 0, 10) + ' million', color = 0, /device
endif else begin
 xyouts, 10, 15, 'Total cost = $ ' + strmid(strtrim(string(round(cost_total_def/1.e9)), 1), 0, 10) + ' billion', color = 0, /device
endelse

; make the 'filter'
red = floor(0.9*wavelength*1000.) ; in nanometers
blue = floor(1.1*wavelength*1000.) ; in nanometers
filter = fltarr(3000)
filter(red:blue) = 1.

; show the spectrum
wset, plot_def
plot, findgen(3000)/1000., spectrum_out_def/max(spectrum_out_def[500:2999]), xrange=[0.5, 3.], xtitle = 'Wavelength (microns)', ytitle = 'Normalized flux', charsize = 1., thick = 1., color = 0 
oplot, findgen(3000)/1000., filter, thick = 2., color = 0

; show the images
wset, images_def
image = congrid(image_out_def, 128, 128)
image_ao = congrid(image_ao_out_def, 128, 128)
tvscl, alog10(zero(image) + 0.001), 0, 0
tvscl, alog10(zero(image_ao) + 0.001), 128, 0
; and add some information
blank = fltarr(256, 40) + 255.
tv, blank, 0, 128
xyouts, 10, 150, 'No-AO, ' + strmid(strtrim(string(field), 1), 0, 3) + '" X ' + strmid(strtrim(string(field), 1), 0, 3) + '"', color = 0, /device
xyouts, 10, 135, 'Redshift = ' + strmid(strtrim(string(redshift), 1), 0, 4), color = 0, /device
xyouts, 140, 150, 'Strehl ratio = ' + strmid(strtrim(string(strehl), 1), 0, 4), color = 0, /device
xyouts, 140, 135, 'B/T = ' + strmid(strtrim(string(bulge_to_total_def), 1), 0, 8), color = 0, /device

end

;--------------------------------------
pro show_results

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def, $
                 deceleration_def, $
                 magnitude_def, bulge_radius_def, bulge_to_total_given_def, inclination_def, redshift_def, fixed_def, galaxies_def, metric_def
                 
common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   deceleration_min, $
                   magnitude_min, bulge_radius_min, bulge_to_total_given_min, inclination_min, redshift_min, galaxies_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max, $
                   deceleration_max, $
                   magnitude_max, bulge_radius_max, bulge_to_total_given_max, inclination_max, redshift_max, galaxies_max

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

common inputs, field, perfect_bulge, perfect_disk, perfect_spectrum, background_spectrum

common pictures, image_ao_out, image_ao_out_def

common outputs, cost_total, cost_total_def, snr, snr_def, average_snr, average_snr_def

; set some values
if site eq 0 then begin ; a mid-latitude site
 altitude_tmp = altitude
 road_length_tmp = road_length
 road_cost_tmp = road_cost
 enclosure_cost_tmp = enclosure_cost
 telescope_cost_tmp = telescope_cost
 polar_cost_tmp = polar_cost
 polar_background_tmp = polar_background
 polar_natural_tmp = polar_natural
 polar_ground_tmp = polar_ground
endif
if site eq 1 then begin ; an antarctic site
 if site_def eq 1 then begin ; if the default site is also an antarctic one
  polar_cost_tmp = 1.
  polar_background_tmp = 1.
  polar_natural_tmp = 1.
  polar_ground_tmp = 1.
 endif else begin
  polar_cost_tmp = polar_cost
  polar_background_tmp = polar_background
  polar_natural_tmp = polar_natural
  polar_ground_tmp = polar_ground
 endelse
 altitude_tmp = 3300.
 road_length_tmp = 1100.
 road_cost_tmp = polar_cost*road_cost
 enclosure_cost_tmp = polar_cost*enclosure_cost
 telescope_cost_tmp = polar_cost*telescope_cost
endif
if site eq 2 then begin ; an arctic site
  if site_def eq 1 then begin ; if the default site is an antarctic one
  polar_cost_tmp = 1.
  polar_background_tmp = 1.
  polar_natural_tmp = 1.
  polar_ground_tmp = 1.
 endif else begin
  polar_cost_tmp = polar_cost
  polar_background_tmp = polar_background
  polar_natural_tmp = polar_natural
  polar_ground_tmp = polar_ground
 endelse
 altitude_tmp = 1900.
 road_length_tmp = 50.
 road_cost_tmp = polar_cost*road_cost
 enclosure_cost_tmp = polar_cost*enclosure_cost
 telescope_cost_tmp = polar_cost*telescope_cost
endif

; reset the sliders
widget_control, slider_altitude, set_value = altitude_tmp
widget_control, slider_road_length, set_value = road_length_tmp
widget_control, slider_diameter, set_value = diameter ; needs to be reset for the mapping function
widget_control, slider_polar_cost, set_value = polar_cost_tmp
widget_control, slider_polar_background, set_value = polar_background_tmp
widget_control, slider_polar_natural, set_value = polar_natural_tmp
widget_control, slider_polar_ground, set_value = polar_ground_tmp
; resetting the cost sliders can been shut off since it does not work well when the site is set to polar, but the interface makes more sense with it turned on 
;widget_control, slider_road_cost, set_value = road_cost_tmp
;widget_control, slider_enclosure_cost, set_value = enclosure_cost_tmp
;widget_control, slider_telescope_cost, set_value = telescope_cost_tmp

; build pictures of the observatory
build_observatory, observatory, site, altitude_tmp, altitude_max, diameter, focal_ratio, pier_height

; calculate the diffraction limit for the observatory
calculate_diffraction, diffraction, diameter, wavelength

; normalize the galaxy spectrum for the observatory
normalize_spectrum, normalized_spectrum, diameter, deceleration, magnitude, redshift, perfect_spectrum

; calculate the signal for the observatory
calculate_signal, signal, wavelength, normalized_spectrum

; calculate the background for the observatory
calculate_background, site_spectrum, background, site, diameter, wavelength, polar_background, background_spectrum

; make a perfect images for the observatory
make_images, bulge, galaxy, diffraction, deceleration, signal, field, perfect_bulge, perfect_disk, bulge_radius, bulge_to_total_given, inclination, redshift

; calculate the seeing for the observatory
calculate_seeing, seeing_site, seeing_total, site, polar_natural, polar_ground, altitude, diameter, pier_height, wavelength

; degrade the perfect image for the observatory
degrade_images, image_out, image_ao_out, field, diffraction, seeing_total, galaxy, background, strehl

; make the spectrum for the observatory
make_spectrum, spectrum_out, wavelength, normalized_spectrum, site_spectrum

; measure the bulge-to-total ratio for the observatory
measure_bulge, bulge_to_total, bulge_to_total_error, image_ao_out, bulge, galaxy, bulge_to_total_given

; calculate the costs for the observatory
calculate_costs, cost_site, cost_telescope, cost_total, road_length_tmp, diameter, focal_ratio, pier_height, wavelength, seeing_total, road_cost_tmp, enclosure_cost_tmp, telescope_cost_tmp, actuator_cost, strehl

; measure the structure recovery for the observatory and default observatory
measure_structure, structure, image_ao_out, image_ao_out_def, galaxy, bulge

; calculate the signal-to-noise ratio for the observatory
if metric eq 0 then begin
 snr = structure
endif else begin
 ; use the bulge-to-total ratio instead
 snr = bulge_to_total_given/bulge_to_total_error
endelse

; calcuate the figures of merit
fom = snr/cost_total
fom_def = snr_def/cost_total_def

; calculate the relative values
delta_cost = cost_total/cost_total_def
delta_snr = snr/snr_def
delta_fom = fom/fom_def

; display the results

; show the picture
wset, picture
tvscl, observatory, 0, 0

; show some further information with the pictures
; telescope
tmp = ['a mid-latitude', 'an antarctic', 'an arctic']
xyouts, 10, 150, 'Proposed observatory at ' + tmp(site) + ' site', color = 0, /device
; altitude
xyouts, 10, 135, 'Altitude = ' + strmid(strtrim(string(altitude_tmp), 1), 0, 6) + ' m', color = 0, /device
; wavelength
xyouts, 10, 120, 'Wavelength = ' + strmid(strtrim(string(wavelength), 1), 0, 3) + ' microns', color = 0, /device
; seeing
xyouts, 10, 105, 'Seeing = ' + strmid(strtrim(string(seeing_total), 1), 0, 4) + ' arcsec', color = 0, /device
; road length
xyouts, 10, 90, 'Road length = ' + strmid(strtrim(string(road_length_tmp), 1), 0, 6) + ' km', color = 0, /device
; telescope
xyouts, 10, 75, 'Telescope', color = 0, /device
; diameter
xyouts, 10, 60, 'Diameter = ' + strmid(strtrim(string(diameter), 1), 0, 5) + ' m', color = 0, /device
; focal ratio
xyouts, 10, 45, 'Focal ratio = ' + strmid(strtrim(string(focal_ratio), 1), 0, 3), color = 0, /device
; pier height
xyouts, 10, 30, 'Pier height = ' + strmid(strtrim(string(pier_height), 1), 0, 4) + ' m', color = 0, /device
; cost
if round(cost_total/1.e6) lt 1000 then begin
 xyouts, 10, 15, 'Total cost = $ ' + strmid(strtrim(string(round(cost_total/1.e6)), 1), 0, 10) + ' million', color = 0, /device
endif else begin
 xyouts, 10, 15, 'Total cost = $ ' + strmid(strtrim(string(round(cost_total/1.e9)), 1), 0, 10) + ' billion', color = 0, /device
endelse

; relative to the default observatory
xyouts, 150, 135, 'Relative to default', color = 0, /device
xyouts, 150, 120, '(AO-corrected)', color = 0, /device
xyouts, 150, 105, 'SNR = ' + strmid(strtrim(string(delta_snr), 1), 0, 8), color = 0, /device 
xyouts, 150, 90, 'Cost = ' + strmid(strtrim(string(delta_cost), 1), 0, 8), color = 0, /device 
xyouts, 150, 75, 'SNR/$ = ' + strmid(strtrim(string(delta_fom), 1), 0, 8), color = 0, /device 

; make the 'filter'
red = floor(0.9*wavelength*1000.) ; in nanometers
blue = floor(1.1*wavelength*1000.) ; in nanometers
filter = fltarr(3000)
filter(red:blue) = 1.

; show the spectrum
wset, plot
plot, findgen(3000)/1000., spectrum_out/max(spectrum_out[500:2999]), xrange=[0.5, 3.], xtitle = 'Wavelength (microns)', ytitle = 'Normalized flux', charsize = 1., thick = 1., color = 0 
oplot, findgen(3000)/1000., filter, thick = 2., color = 0

; show the images
wset, images
image = congrid(image_out, 128, 128)
image_ao = congrid(image_ao_out, 128, 128)
tvscl, alog10(zero(image) + 0.001), 0, 0
tvscl, alog10(zero(image_ao) + 0.001), 128, 0
; and add some information
blank = fltarr(256, 40) + 255.
tv, blank, 0, 128
xyouts, 10, 150, 'No-AO, ' + strmid(strtrim(string(field), 1), 0, 3) + '" X ' + strmid(strtrim(string(field), 1), 0, 3) + '"', color = 0, /device
xyouts, 10, 135, 'Redshift = ' + strmid(strtrim(string(redshift), 1), 0, 4), color = 0, /device
xyouts, 140, 150, 'Strehl ratio = ' + strmid(strtrim(string(strehl), 1), 0, 4), color = 0, /device
xyouts, 140, 135, 'B/T = ' + strmid(strtrim(string(bulge_to_total), 1), 0, 8), color = 0, /device

end

;--------------------------------------
pro take_survey

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def, $
                 deceleration_def, $
                 magnitude_def, bulge_radius_def, bulge_to_total_given_def, inclination_def, redshift_def, fixed_def, galaxies_def, metric_def
                 
common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   deceleration_min, $
                   magnitude_min, bulge_radius_min, bulge_to_total_given_min, inclination_min, redshift_min, galaxies_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max, $
                   deceleration_max, $
                   magnitude_max, bulge_radius_max, bulge_to_total_given_max, inclination_max, redshift_max, galaxies_max

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

common outputs, cost_total, cost_total_def, snr, snr_def, average_snr, average_snr_def

; set up the display
if site_def eq 0 then begin ; mid-latitude site
 color_def = 255
endif else begin ; polar site
 color_def = 0
endelse
if site eq 0 then begin ; mid-latitude site
 color = 255
endif else begin ; polar site
 color = 0
endelse

; take a survey
total_snr = 0.
total_snr_def = 0.
for k = 1, galaxies do begin
 
 ; generate random values
 bulge_radius_tmp = bulge_radius_min + randomu(seed)*(bulge_radius_max - bulge_radius_min)
 bulge_to_total_given_tmp = bulge_to_total_given_min + randomu(seed)*(bulge_to_total_given_max - bulge_to_total_given_min)
 inclination_tmp = inclination_min + randomu(seed)*(inclination_max - inclination_min)
; print, bulge_radius_tmp, bulge_to_total_given_tmp, inclination_tmp

 ; set the fixed value or values
 if fixed eq 0 then begin ; bulge radius
;  bulge_radius = bulge_radius_tmp
  bulge_to_total_given = bulge_to_total_given_tmp
  inclination = inclination_tmp
 endif
 if fixed eq 1 then begin ; bulge-to-total ratio
  bulge_radius = bulge_radius_tmp
;  bulge_to_total_given = bulge_to_total_given_tmp
  inclination = inclination_tmp
 endif
 if fixed eq 2 then begin ; inclination
  bulge_radius = bulge_radius_tmp
  bulge_to_total_given = bulge_to_total_given_tmp
;  inclination = inclination_tmp
 endif
 if fixed eq 3 then begin ; all
;  bulge_radius = bulge_radius_tmp
;  bulge_to_total_given = bulge_to_total_given_tmp
;  inclination = inclination_tmp
 endif
 if fixed eq 4 then begin ; none
  bulge_radius = bulge_radius_tmp
  bulge_to_total_given = bulge_to_total_given_tmp
  inclination = inclination_tmp
 endif

 ; reset the sliders
 widget_control, slider_magnitude, set_value = magnitude ; this needs to be reset, even though it is not randomized
 widget_control, slider_bulge_radius, set_value = bulge_radius
 widget_control, slider_bulge_to_total_given, set_value = bulge_to_total_given
 widget_control, slider_inclination, set_value = inclination
 widget_control, slider_redshift, set_value = redshift ; this needs to be reset, even though it is not randomized
 ; and other controls
; widget_control, slider_wavelength, set_value = wavelength ; must be reset due to comparison feature
 widget_control, toggle_site, set_value = site ; must be reset due to comparison feature
 widget_control, slider_diameter, set_value = diameter ; must be reset due to comparison feature

 ; give a progress report on number of galaxies surveyed
 wset, picture
 xyouts, 150, 60, strmid(strtrim(string(k), 1), 0, 5) + ' of ' + strmid(strtrim(string(galaxies), 1), 0, 5), color = 0, /device

 ; generate results
 show_results_def
 show_results

 ; sum
 total_snr = total_snr + snr
 total_snr_def = total_snr_def + snr_def

endfor
average_snr = total_snr/float(galaxies)
average_snr_def = total_snr_def/float(galaxies)

; calcuate the figures of merit
fom = average_snr/cost_total
fom_def = average_snr_def/cost_total_def

; calculate the relative values
delta_cost = cost_total/cost_total_def
delta_snr = average_snr/average_snr_def
delta_fom = fom/fom_def

; show results relative to the default observatory
wset, picture
xyouts, 150, 45, '(Survey average', color = 0, /device
xyouts, 150, 30, 'SNR = ' + strmid(strtrim(string(delta_snr), 1), 0, 8), color = 0, /device
xyouts, 150, 15, 'SNR/$ = ' + strmid(strtrim(string(delta_fom), 1), 0, 8) + ')', color = 0, /device

end

;--------------------------------------
pro map_parameter_space

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

common outputs, cost_total, cost_total_def, snr, snr_def, average_snr, average_snr_def

common answers, delta_cost, delta_snr, delta_fom

; go through each magnitude and redshift
magnitudes = [-14., -15., -16., -17., -18., -19., -20., -21., -22., -23., -24., -25., -26.]
;redshifts = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1., 2., 3., 4., 5., 6., 7., 8., 9., 10.]
;redshifts = [1., 2., 3., 4., 5., 6., 7., 8., 9., 10.]
redshifts = [1., 2., 3., 4., 5.]
;global_snr = fltarr(13, 19)
;global_snr = fltarr(13, 10)
global_snr = fltarr(13, 5)
global_snr_def = global_snr
for l = 0, 12 do begin
; for m = 0, 18 do begin
; for m = 0, 9 do begin
 for m = 0, 4 do begin

  ; set the magnitude and redshift
  magnitude = magnitudes(l)
  redshift = redshifts(m)

  ; run in survey mode
  take_survey

  ; record the results
  global_snr(l, m) = average_snr
  global_snr_def(l, m) = average_snr_def

 endfor
endfor

; print out the results to a file
get_lun, unit
openw, unit, 'results.dat', /append
for l = 0, 12 do begin
; printf, unit, format = '(19f)', magnitudes(l), (global_snr(l, *)/cost_total)/(global_snr_def(l, *)/cost_total_def)
; printf, unit, format = '(10f)', magnitudes(l), (global_snr(l, *)/cost_total)/(global_snr_def(l, *)/cost_total_def)
 printf, unit, format = '(5f)', magnitudes(l), (global_snr(l, *)/cost_total)/(global_snr_def(l, *)/cost_total_def)
endfor
close, unit
free_lun, unit

; transpose these data to correspond to increasing redshift to the right and increasing brightness up in a 3-d plot
global_snr = transpose(global_snr)
global_snr_def = transpose(global_snr_def)

;; calcuate the figures of merit
;fom = global_snr/cost_total
;fom_def = global_snr_def/cost_total_def

;; calculate the relative values
;delta_cost = cost_total/cost_total_def
;delta_snr = global_snr/global_snr_def
;delta_fom = fom/fom_def

; show results

; SNR for the default observatory
wset, plot_def
image = congrid(global_snr_def, 256, 128)
image[0,0] = max(global_snr)
image[0,1] = min(global_snr)
tvscl, alog10(zero(image) + 0.001), 0, 0
; and add some information
blank = fltarr(256, 25) + 255.
tv, blank, 0, 128
xyouts, 10, 135, 'SNR (z, M_B), mean = ' + strmid(strtrim(string(mean(global_snr_def, /NAN)), 1), 0, 8), color = 0, /device

; SNR for the observatory
wset, plot
image = congrid(global_snr, 256, 128)
image[0,0] = max(global_snr_def)
image[0,1] = min(global_snr_def)
tvscl, alog10(zero(image) + 0.001), 0, 0
; and add some information
blank = fltarr(256, 25) + 255.
tv, blank, 0, 128
xyouts, 10, 135, 'SNR (z, M_B), mean = ' + strmid(strtrim(string(mean(global_snr, /NAN)), 1), 0, 8), color = 0, /device

; calculate mean signal-to-noise
global_snr = mean(global_snr, /NAN)
global_snr_def = mean(global_snr_def, /NAN)

; calcuate the figures of merit
fom = global_snr/cost_total
fom_def = global_snr_def/cost_total_def

; calculate the relative values
delta_cost = cost_total/cost_total_def
delta_snr = global_snr/global_snr_def
delta_fom = fom/fom_def

; relative SNR
wset, images_def
blank = fltarr(256, 128) + 255.
tv, blank, 0, 0
;image = congrid(delta_snr, 256, 128)
;tvscl, alog10(zero(image) + 0.001), 0, 0
; and add some information
blank = fltarr(256, 40) + 255.
tv, blank, 0, 128
;xyouts, 10, 135, 'Relative SNR (z, M_B), mean = ' + strmid(strtrim(string(mean(delta_snr, /NAN)), 1), 0, 8), color = 0, /device
;xyouts, 10, 135, 'Relative SNR (z, M_B), mean = ' + strmid(strtrim(string(delta_snr), 1), 0, 8), color = 0, /device

; relative SNR/$
wset, images
blank = fltarr(256, 128) + 255.
tv, blank, 0, 0
;image = congrid(delta_fom, 256, 128)
;tvscl, alog10(zero(image) + 0.001), 0, 0
; and add some information
blank = fltarr(256, 40) + 255.
tv, blank, 0, 128
;xyouts, 10, 135, 'Relative SNR/$ (z, M_B), mean = ' + strmid(strtrim(string(mean(delta_fom, /NAN)), 1), 0, 8), color = 0, /device
xyouts, 10, 150, 'Relative SNR (z, M_B), mean = ' + strmid(strtrim(string(delta_snr), 1), 0, 8), color = 0, /device
xyouts, 10, 135, 'Relative SNR/$ (z, M_B), mean = ' + strmid(strtrim(string(delta_fom), 1), 0, 8), color = 0, /device

end

;--------------------------------------
pro compare_telescopes

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

common answers, delta_cost, delta_snr, delta_fom

; go through each telescope and site
diameters = [10., 20., 30., 50., 100.]
focal_ratios = [1.8, 1., 1., 1., 1.]
pier_heights = [10., 20., 20., 10., 0.]
sites = [0, 1, 2]
;wavelengths = [2.2, 2.4, 2.4]
table_cost = fltarr(5, 3)
table_snr = table_cost
table_fom = table_cost
for p = 0, 4 do begin ; telescope aperture
 for q = 0, 2 do begin ; sites

  ; set the diameter, focal ratio, pier height, and site
  diameter = diameters(p)
  focal_ratio = focal_ratios(p)
  pier_height = pier_heights(p)
  site = sites(q)
;  wavelength = wavelengths(q)
  
  ; map parameter space
  map_parameter_space

  ; record the answers
  table_cost(p, q) = delta_cost
  table_snr(p, q) = delta_snr
  table_fom(p, q) = delta_fom

 endfor
endfor

; print out the tables to a file
get_lun, unit
openw, unit, 'table.dat'
printf, unit, 'Diameter, mid-latitude, antarctic, arctic'
printf, unit, 'Delta(cost)'
for p = 0, 4 do begin
 printf, unit, format = '(4f)', diameters(p), table_cost(p, *)
endfor
printf, unit, 'Delta(SNR)'
for p = 0, 4 do begin
 printf, unit, format = '(4f)', diameters(p), table_snr(p, *)
endfor
printf, unit, 'Delta(FOM)'
for p = 0, 4 do begin
 printf, unit, format = '(4f)', diameters(p), table_fom(p, *)
endfor
close, unit
free_lun, unit

end

;--------------------------------------
pro world_observatory_v21

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def, $
                 deceleration_def, $
                 magnitude_def, bulge_radius_def, bulge_to_total_given_def, inclination_def, redshift_def, fixed_def, galaxies_def, metric_def
                 
common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   deceleration_min, $
                   magnitude_min, bulge_radius_min, bulge_to_total_given_min, inclination_min, redshift_min, galaxies_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max, $
                   deceleration_max, $
                   magnitude_max, bulge_radius_max, bulge_to_total_given_max, inclination_max, redshift_max, galaxies_max

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

common inputs, field, perfect_bulge, perfect_disk, perfect_spectrum, background_spectrum

; shut off arithmetic error messages.
!except = 0

; set the background color to white.
!p.background = 255

;; set up the plot display
;!p.multi = [0, 1, 1]

; set parameter values to the defaults
set_defaults, 0

; build the widget display and control panel
widget_control, /reset

; display
display = widget_base(/row, title = 'World Observatory')
output = widget_base(display, /column)
pictures = widget_base(output, /row)
picture_def = widget_draw(pictures, xsize = 256, ysize = 384)
picture = widget_draw(pictures, xsize = 256, ysize = 384)
plots = widget_base(output, /row)
plot_def = widget_draw(plots, xsize = 256, ysize = 128 + 25)
plot = widget_draw(plots, xsize = 256, ysize = 128 + 25) 
results = widget_base(output, /row)
images_def = widget_draw(results, xsize = 256, ysize = 128 + 40)
images = widget_draw(results, xsize = 256, ysize = 128 + 40)

; controls
controls = widget_base(display, /column)

; top row
top = widget_base(controls, /row)

; 'Site'
controls_site = widget_base(top, /column)
controls_site_top = widget_base(controls_site, /column)
label = widget_label(controls_site_top, value = 'Site', xsize = 300)
toggle_site = cw_bgroup(controls_site_top, ['Mid-latitude', 'Antarctic', 'Arctic'], set_value = site_def, /exclusive, /row, uvalue='site')
controls_site_top_first = widget_base(controls_site_top, /row) 
slider_altitude = cw_fslider(controls_site_top_first, maximum = altitude_max, minimum = altitude_min, uvalue = 'altitude', title = 'Altitude (m)', value = altitude_def, xsize = 150, drag = 1)
slider_road_length = cw_fslider(controls_site_top_first, maximum = road_length_max, minimum = road_length_min, uvalue = 'road_length', title = 'Road length (km)', value = road_length_def, xsize = 150, drag = 1)
label = widget_label(controls_site_top, value = 'Polar factors', xsize = 300)
controls_site_bottom = widget_base(controls_site, /row)
controls_site_bottom_first = widget_base(controls_site_bottom, /column)
slider_polar_cost = cw_fslider(controls_site_bottom_first, maximum = polar_cost_max, minimum = polar_cost_min, uvalue = 'polar_cost', title = 'Increased cost', value = polar_cost, xsize = 150, drag = 1)
slider_polar_background = cw_fslider(controls_site_bottom_first, maximum = polar_background_max, minimum = polar_background_min, uvalue = 'polar_background', title = 'Decreased background', value = polar_background, xsize = 150, drag = 1)
controls_site_bottom_second = widget_base(controls_site_bottom, /column)
slider_polar_natural = cw_fslider(controls_site_bottom_second, maximum = polar_natural_max, minimum = polar_natural_min, uvalue = 'polar_natural', title = 'Improved free seeing', value = polar_natural, xsize = 150, drag = 1)
slider_polar_ground = cw_fslider(controls_site_bottom_second, maximum = polar_ground_max, minimum = polar_ground_min, uvalue = 'polar_ground', title = 'Improved ground seeing', value = polar_ground, xsize = 150, drag = 1)

; 'Telescope'
controls_telescope = widget_base(top, /column)
label = widget_label(controls_telescope, value = 'Telescope', xsize = 150)
slider_diameter = cw_fslider(controls_telescope, maximum = diameter_max, minimum = diameter_min, uvalue = 'diameter', title = 'Diameter (m)', value = diameter_def, xsize = 150, drag = 1)
slider_focal_ratio = cw_fslider(controls_telescope, maximum = focal_ratio_max, minimum = focal_ratio_min, uvalue = 'focal_ratio', title = 'Focal ratio', value = focal_ratio_def, xsize = 150, drag = 1)
slider_pier_height = cw_fslider(controls_telescope, maximum = pier_height_max, minimum = pier_height_min, uvalue = 'pier_height', title = 'Pier height (m)', value = pier_height_def, xsize = 150, drag = 1)
slider_wavelength = cw_fslider(controls_telescope, maximum = wavelength_max, minimum = wavelength_min, uvalue = 'wavelength', title = 'Wavelength (microns)', value = wavelength_def, xsize = 150, drag = 1)
;slider_strehl = cw_fslider(controls_telescope, maximum = strehl_max, minimum = strehl_min, uvalue = 'strehl', title = 'Strehl ratio', value = strehl_def, xsize = 150, drag = 1)

; 'Costs'
controls_costs = widget_base(top, /column)
label = widget_label(controls_costs, value = 'Costs', xsize = 150)
slider_road_cost = cw_fslider(controls_costs, maximum = road_cost_max, minimum = road_cost_min, uvalue = 'road_cost', title = 'Road ($/km)', value = road_cost_def, xsize = 150, drag = 1)
;slider_enclosure_cost = cw_fslider(controls_costs, maximum = enclosure_cost_max, minimum = enclosure_cost_min, uvalue = 'enclosure_cost', title = 'Enclosure ($/m^3)', value = enclosure_cost_def, xsize = 150, drag = 1)
slider_enclosure_cost = cw_fslider(controls_costs, maximum = enclosure_cost_max, minimum = enclosure_cost_min, uvalue = 'enclosure_cost', title = 'Enclosure ($/m^2)', value = enclosure_cost_def, xsize = 150, drag = 1)
slider_telescope_cost = cw_fslider(controls_costs, maximum = telescope_cost_max, minimum = telescope_cost_min, uvalue = 'telescope_cost', title = 'Telescope ($/m^2)', value = telescope_cost_def, xsize = 150, drag = 1)
slider_actuator_cost = cw_fslider(controls_costs, maximum = actuator_cost_max, minimum = actuator_cost_min, uvalue = 'actuator_cost', title = 'Adaptive optics ($/dof)', value = actuator_cost_def, xsize = 150, drag = 1)

; bottom row
bottom = widget_base(controls, /row)

; 'Default observatory'
controls_default_telescope = widget_base(bottom, /column)
label = widget_label(controls_default_telescope, value = 'Default observatory', xsize = 150)
toggle_telescope = cw_bgroup(controls_default_telescope, ['DMT on Dome C', 'CFHT on Mauna Kea', 'Gemini on Mauna Kea', 'Gemini on Cerro Pachon', 'Keck on Mauna Kea', 'VLOT on Mauna Kea', 'CELT on Mauna Kea', 'Euro50 on La Palma', 'OWL on La Palma'], set_value = telescope_def, /exclusive, /column, uvalue='telescope')
slider_strehl = cw_fslider(controls_default_telescope, maximum = strehl_max, minimum = strehl_min, uvalue = 'strehl', title = 'Strehl ratio', value = strehl_def, xsize = 150, drag = 1)

;; 'Cosmology'
;label = widget_label(controls_default_telescope, value = 'Cosmology', xsize = 150)
;slider_deceleration = cw_fslider(controls_default_telescope, maximum = deceleration_max, minimum = deceleration_min, uvalue = 'deceleration', title = 'Deceleration parameter', value = deceleration_def, xsize = 150, drag = 1)

; 'Galaxy'
controls_galaxy = widget_base(bottom, /column)
label = widget_label(controls_galaxy, value = 'Galaxy', xsize = 150)
slider_magnitude = cw_fslider(controls_galaxy, maximum = magnitude_max, minimum = magnitude_min, uvalue = 'magnitude', title = 'Absolute B magnitude', value = magnitude_def, xsize = 150, drag = 1)
slider_bulge_radius = cw_fslider(controls_galaxy, maximum = bulge_radius_max, minimum = bulge_radius_min, uvalue = 'bulge_radius', title = 'Bulge radius (kpc)', value = bulge_radius_def, xsize = 150, drag = 1)
slider_bulge_to_total_given = cw_fslider(controls_galaxy, maximum = bulge_to_total_given_max, minimum = bulge_to_total_given_min, uvalue = 'bulge_to_total_given', title = 'Bulge-to-total ratio', value = bulge_to_total_given_def, xsize = 150, drag = 1)
slider_inclination = cw_fslider(controls_galaxy, maximum = inclination_max, minimum = inclination_min, uvalue = 'inclination', title = 'Inclination (degrees)', value = inclination_def, xsize = 150, drag = 1)

; 'Survey'
controls_survey = widget_base(bottom, /column)
label = widget_label(controls_survey, value = 'Survey at fixed', xsize = 150)
slider_redshift = cw_fslider(controls_survey, maximum = redshift_max, minimum = redshift_min, uvalue = 'redshift', title = 'Redshift', value = redshift_def, xsize = 150, drag = 1)
toggle_fixed = cw_bgroup(controls_survey, ['Bulge radius', 'Bulge-to-total ratio', 'Inclination', 'All of the above', 'None of the above'], set_value = fixed_def, /exclusive, /column, uvalue='fixed')
slider_galaxies = widget_slider(controls_survey, maximum = galaxies_max, minimum = galaxies_min, uvalue = 'galaxies', title='Number of galaxies', value = galaxies_def, xsize = 150) 
toggle_metric = cw_bgroup(controls_survey, ['Measure structure', 'Measure bulge'], set_value = metric_def, /exclusive, /column, uvalue='metric')

; buttons
buttons = widget_base(bottom, /column)

; 'Cosmology'
label = widget_label(buttons, value = 'Cosmology', xsize = 150)
slider_deceleration = cw_fslider(buttons, maximum = deceleration_max, minimum = deceleration_min, uvalue = 'deceleration', title = 'Deceleration parameter', value = deceleration_def, xsize = 150, drag = 1)

button_survey = cw_bgroup(buttons, ['Take galaxy survey'], /column, xsize = 150, uvalue = 'button_survey')
button_map = cw_bgroup(buttons, ['Map parameter space'], /column, xsize = 150, uvalue = 'button_map')
button_compare = cw_bgroup(buttons, ['Compare obseratories'], /column, xsize = 150, uvalue = 'button_compare')
button = cw_bgroup(buttons, ['Help', 'Reset all', 'About', 'Quit'], /column, xsize = 100, uvalue = 'buttons')

; realize the display and controls
widget_control, /realize, display
widget_control, /realize, controls
widget_control, picture, get_value = picture
widget_control, picture_def, get_value = picture_def
widget_control, images, get_value = images
widget_control, images_def, get_value = images_def
widget_control, plot, get_value = plot
widget_control, plot_def, get_value = plot_def

; show the default results
show_results_def
show_results

; submit the control widget to the xmanager
xmanager, 'world_observatory_v21_controls', controls

end

;--------------------------------------
pro world_observatory_v21_controls_event, event

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost, $
                   deceleration, $
                   magnitude, bulge_radius, bulge_to_total_given, inclination, redshift, fixed, galaxies, metric

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  slider_deceleration, $
                  slider_magnitude, slider_bulge_radius, slider_bulge_to_total_given, slider_inclination, slider_redshift, toggle_fixed, slider_galaxies, toggle_metric, $
                  picture, picture_def, images, images_def, plot, plot_def

; monitor the controls
widget_control, get_uvalue = uvalue, event.id

; action with the sliders and toggles
if uvalue eq 'site' then site = event.value
if uvalue eq 'polar_cost' then polar_cost = event.value
if uvalue eq 'polar_background' then polar_background = event.value
if uvalue eq 'polar_natural' then polar_natural = event.value
if uvalue eq 'polar_ground' then polar_ground = event.value
if site eq 0 then begin ; if this is a polar site the next two values won't be changed
 if uvalue eq 'altitude' then altitude = event.value
 if uvalue eq 'road_length' then road_length = event.value
endif
if uvalue eq 'telescope' then begin ; set a new default telescope
 telescope = event.value
 set_defaults, 1
; ; this can be shut off in order to allow switch of default without change of observatory
; widget_control, toggle_site, set_value = site
; widget_control, slider_altitude, set_value = altitude
; widget_control, slider_road_length, set_value = road_length
; widget_control, slider_diameter, set_value = diameter
; widget_control, slider_focal_ratio, set_value = focal_ratio
; widget_control, slider_pier_height, set_value = pier_height
 show_results_def
endif
if uvalue eq 'diameter' then diameter = event.value
if uvalue eq 'focal_ratio' then focal_ratio = event.value
if uvalue eq 'pier_height' then pier_height = event.value
if uvalue eq 'wavelength' then begin ; for changes in wavelength, the default needs to be reset as well
 wavelength = event.value
 show_results_def
endif
if uvalue eq 'strehl' then begin ; for changes in Strehl ratio, the default needs to be reset as well
 strehl = event.value
 show_results_def
endif
if uvalue eq 'road_cost' then begin ; for changes in road cost, the default needs to be reset as well
 road_cost = event.value
 show_results_def
endif
if uvalue eq 'enclosure_cost' then begin ; for changes in enclosure cost, the default needs to be reset as well
 enclosure_cost = event.value
 show_results_def
endif
if uvalue eq 'telescope_cost' then begin ; for changes in telescope cost, the default needs to be reset as well
 telescope_cost = event.value
 show_results_def
endif
if uvalue eq 'actuator_cost' then begin ; for changes in actuator cost, the default needs to be reset as well
 actuator_cost = event.value
 show_results_def
endif
if uvalue eq 'deceleration' then begin ; for changes in deceleration parameter, the default needs to be reset as well
 deceleration = event.value
 show_results_def
endif
if uvalue eq 'magnitude' then begin ; for changes in magnitude, the default needs to be reset as well
 magnitude = event.value
 show_results_def
endif
if uvalue eq 'bulge_radius' then begin ; for changes in bulge radius, the default needs to be reset as well
 bulge_radius = event.value
 show_results_def
endif
if uvalue eq 'bulge_to_total_given' then begin ; for changes in bulge-to-total ratio, the default needs to be reset as well
 bulge_to_total_given = event.value
 show_results_def
endif
if uvalue eq 'inclination' then begin ; for changes in inclination, the default needs to be reset as well
 inclination = event.value
 show_results_def
endif
if uvalue eq 'redshift' then begin ; for changes in redshift, the default needs to be reset as well
 redshift = event.value
 show_results_def
endif
if uvalue eq 'fixed' then fixed = event.value
if uvalue eq 'galaxies' then galaxies = event.value
if uvalue eq 'metric' then begin ; for changes in metric, the default needs to be reset as well
 metric = event.value
 show_results_def
endif

; show the results
show_results

; action with the survey button
if uvalue eq 'button_survey' then begin 
 if event.value eq 0 then take_survey
endif

; action with the mapping button
if uvalue eq 'button_map' then begin 
 if event.value eq 0 then map_parameter_space
endif

; action with the comparison button
if uvalue eq 'button_compare' then begin 
 if event.value eq 0 then compare_telescopes
endif

; action with the other buttons
if uvalue eq 'buttons' then begin
 if event.value eq 0 then xdisplayfile, 'world_observatory_v21.doc', title = 'World Observatory Help', group = event.top, height = 30, width = 75
 if event.value eq 1 then begin ; global reset of parameters, including the defaults
  set_defaults, 0
  widget_control, toggle_site, set_value = site
  widget_control, slider_polar_cost, set_value = polar_cost
  widget_control, slider_polar_background, set_value = polar_background
  widget_control, slider_polar_natural, set_value = polar_natural
  widget_control, slider_polar_ground, set_value = polar_ground
  widget_control, slider_altitude, set_value = altitude
  widget_control, slider_road_length, set_value = road_length
  widget_control, toggle_telescope, set_value = telescope
  widget_control, slider_diameter, set_value = diameter
  widget_control, slider_focal_ratio, set_value = focal_ratio
  widget_control, slider_pier_height, set_value = pier_height
  widget_control, slider_wavelength, set_value = wavelength
  widget_control, slider_strehl, set_value = strehl  
  widget_control, slider_road_cost, set_value = road_cost
  widget_control, slider_enclosure_cost, set_value = enclosure_cost
  widget_control, slider_telescope_cost, set_value = telescope_cost
  widget_control, slider_actuator_cost, set_value = actuator_cost
  widget_control, slider_deceleration, set_value = deceleration
  widget_control, slider_magnitude, set_value = magnitude
  widget_control, slider_bulge_radius, set_value = bulge_radius
  widget_control, slider_bulge_to_total_given, set_value = bulge_to_total_given
  widget_control, slider_inclination, set_value = inclination
  widget_control, slider_redshift, set_value = redshift
  widget_control, toggle_fixed, set_value = fixed
  widget_control, slider_galaxies, set_value = galaxies
  widget_control, toggle_metric, set_value = metric
  show_results_def
  show_results
 endif
 if event.value eq 2 then show_information
 if event.value eq 3 then widget_control, /reset
endif

end
