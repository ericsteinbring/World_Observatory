;--------------------------------------
; world_observatory_v11
; Eric Steinbring
; V1.0 29 November 2004
; V1.1 10 December 2005
; See world_observatory_v11.doc for more information.

;--------------------------------------
pro show_information

; print author information to the screen
;erase, 255
blank = fltarr(384, 384) + 255.
tv, blank, 0, 0
xyouts, 192 - 100, 192 + 60, 'World Observatory', charsize = 2.5, color = 0, /device
xyouts, 192 - 100, 192 + 45, 'Eric Steinbring', charsize = 1.25, color = 0, /device
xyouts, 192 - 100, 192 + 30, 'Herzberg Institute for Astrophysics', charsize = 1.25, color = 0, /device
xyouts, 192 - 100, 192 + 15, 'National Research Council Canada', charsize = 1.25, color = 0, /device
xyouts, 192 - 100, 192, 'Victoria, BC, V9E 2E7', charsize = 1.25, color = 0, /device
xyouts, 192 - 100, 192 - 15, 'Eric.Steinbring@nrc-cnrc.gc.ca', charsize = 1.25, color = 0, /device
xyouts, 192 - 100, 192 - 30, 'members.shaw.ca/ericsteinbring', charsize = 1.25, color = 0, /device
xyouts, 192 - 100, 192 - 45, 'Version 1.1: 10 December 2005', charsize = 1.25, color = 0, /device

end

;--------------------------------------
pro set_defaults, parameter

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def

common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max

common counter, first

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

 ; the counter
 first = 1

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

 ; CFHT
 if telescope eq 1 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 3.6 ; meters
  focal_ratio_def = 3.8 ; dimensionless
  pier_height_def = 22. ; meters
 endif

 ; Gemini North
 if telescope eq 2 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 8.1 ; meters
  focal_ratio_def = 1.8 ; dimensionless
  pier_height_def = 21. ; meters
 endif

 ; Gemini South
 if telescope eq 3 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 2700. ; meters
  road_length_def = 50. ; kilometers
  diameter_def = 8.1 ; meters
  focal_ratio_def = 1.8 ; dimensionless
  pier_height_def = 21. ; meters
 endif

 ; Keck
 if telescope eq 4 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 10. ; meters
  focal_ratio_def = 1.8 ; dimensionless
  pier_height_def = 10. ; meters
 endif

 ; VLOT
 if telescope eq 5 then begin
  site_def = 0 ; mid-latitude site
  altitude_def = 4200. ; meters
  road_length_def = 0. ; kilometers
  diameter_def = 20. ; meters
  focal_ratio_def = 1. ; dimensionless
  pier_height_def = 10. ; meters
 endif

 ; CELT
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

 ; OWL
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
altitude_min = 0. ; altitude of the site in meters
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
wavelength_min = 0.4 ; wavelength of observation in microns
wavelength_max = 5.
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
pro calculate, costs, snrs, site, polar_background, polar_natural, polar_ground, altitude, road_length, diameter, focal_ratio, pier_height, wavelength, road_cost, enclosure_cost, telescope_cost, actuator_cost, strehl 

; calculate the seeing, diffraction limit, background and costs
calculate_seeing, seeing_site, seeing_total, site, polar_natural, polar_ground, altitude, diameter, pier_height, wavelength
calculate_diffraction, diffraction, diameter, wavelength
calculate_background, background, site, polar_background, wavelength
calculate_costs, cost_site, cost_telescope, cost_total, road_length, diameter, focal_ratio, pier_height, wavelength, seeing_total, road_cost, enclosure_cost, telescope_cost, actuator_cost, strehl
; costs
costs = fltarr(3)
costs(0) = cost_site
costs(1) = cost_telescope
costs(2) = cost_total
; signal-to-noise ratios, signal is the telescope area divided by the area of the seeing disk, the noise is the square-root of the background
snrs = fltarr(3)
snrs(0) = !pi*(diameter/2.)^2./(seeing_site^2. + diffraction^2.)/sqrt(background) ; at the ground
snrs(1) = !pi*(diameter/2.)^2./(seeing_total^2. + diffraction^2.)/sqrt(background) ; on a pier
snrs(2) = !pi*(diameter/2.)^2./diffraction^2./sqrt(background) ; on a pier, with adaptive optics

end

;--------------------------------------
pro calculate_seeing, seeing_site, seeing_total, site, polar_natural, polar_ground, altitude, diameter, pier_height, wavelength

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
pro calculate_diffraction, diffraction, diameter, wavelength

; the diffraction limited FWHM of the telescope at a given wavelength
diffraction = 206265.*(wavelength*1.e-6/diameter) ; in arcseconds, wavelength converted to meters from microns

end

;--------------------------------------
pro calculate_background, background, site, polar_background, wavelength

; the background for the site
if site eq 0 then begin ; a mid-latitude site
 background = 1.
endif else begin ; a polar site
 if wavelength le 1. then background = 1. ; a linear decrease from the mid-latitude background at 1 microns, wavelength in microns
 if wavelength gt 1. then background = 1./(1. + (polar_background - 1.)*(wavelength - 1.))
endelse

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
pro show_results

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def

common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  pictures, plots

common results, delta_costs, delta_snrs, delta_fom, delta_costs_diffraction, delta_snrs_diffraction, delta_fom_diffraction

common counter, first

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

; build pictures of the default observatory and the observatory
build_observatory, observatory_def, site_def, altitude_def, altitude_max, diameter_def, focal_ratio_def, pier_height_def
build_observatory, observatory, site, altitude_tmp, altitude_max, diameter, focal_ratio, pier_height

; calculate the costs and signal-to-noise ratios for the default observatory and the observatory
calculate, costs_def, snrs_def, site_def, polar_background, polar_natural, polar_ground, altitude_def, road_length_def, diameter_def, focal_ratio_def, pier_height_def, wavelength, road_cost, enclosure_cost, telescope_cost, actuator_cost, strehl 
calculate, costs, snrs, site, polar_background, polar_natural, polar_ground, altitude_tmp, road_length_tmp, diameter, focal_ratio, pier_height, wavelength, road_cost_tmp, enclosure_cost_tmp, telescope_cost_tmp, actuator_cost, strehl

; calculate the change in cost, signal-to-noise ratios, and figures of merit compared to the default telescope
; natural seeing
delta_costs = costs(1)/costs_def(1)
delta_snrs = snrs(1)/snrs_def(1)
fom = snrs(1)/costs(1)
fom_def = snrs_def(1)/costs_def(1)
delta_fom = fom/fom_def
; diffraction limited
delta_costs_diffraction = costs(2)/costs_def(2)
delta_snrs_diffraction = snrs(2)/snrs_def(2)
fom_diffraction = snrs(2)/costs(2)
fom_diffraction_def = snrs_def(2)/costs_def(2)
delta_fom_diffraction = fom_diffraction/fom_diffraction_def

; print this out
;print, site, diameter, delta_costs, delta_snrs, delta_fom, delta_costs_diffraction, delta_snrs_diffraction, delta_fom_diffraction

; show the pictures
wset, pictures
observatory[0:0, *] = 1.
tvscl, observatory_def, 0, 0
tvscl, observatory, 256, 0

; show some further information with the pictures
; telescope
tmp = ['DMT on Dome C', 'CFHT on Mauna Kea', 'Gemini on Mauna Kea', 'Gemini on Cerro Pachon', 'Keck on Mauna Kea', 'VLOT on Mauna Kea', 'CELT on Mauna Kea', 'Euro50 on La Palma', 'OWL on La Palma']
xyouts, 10, 135, tmp(telescope), color = 0, /device
tmp = ['a mid-latitude', 'an antarctic', 'an arctic']
xyouts, 256 + 10, 135, 'Proposed observatory at ' + tmp(site) + ' site', color = 0, /device
; altitude
xyouts, 10, 120, 'Altitude = ' + strmid(strtrim(string(altitude_def), 1), 0, 6) + ' m', color = 0, /device
xyouts, 256 + 10, 120, 'Altitude = ' + strmid(strtrim(string(altitude_tmp), 1), 0, 6) + ' m', color = 0, /device
; wavelength
xyouts, 10, 105, 'Wavelength = ' + strmid(strtrim(string(wavelength), 1), 0, 3) + ' microns', color = 0, /device
xyouts, 256 + 10, 105, 'Wavelength = ' + strmid(strtrim(string(wavelength), 1), 0, 3) + ' microns', color = 0, /device
; seeing
calculate_seeing, seeing_site_def, seeing_total_def, site_def, polar_natural, polar_ground, altitude_def, diameter_def, pier_height_def, wavelength
calculate_seeing, seeing_site, seeing_total, site, polar_natural, polar_ground, altitude_tmp, diameter, pier_height, wavelength
xyouts, 10, 90, 'Seeing = ' + strmid(strtrim(string(seeing_total_def), 1), 0, 4) + ' arcsec', color = 0, /device
xyouts, 256 + 10, 90, 'Seeing = ' + strmid(strtrim(string(seeing_total), 1), 0, 4) + ' arcsec', color = 0, /device
; road length
xyouts, 10, 75, 'Road length = ' + strmid(strtrim(string(road_length_def), 1), 0, 6) + ' km', color = 0, /device
xyouts, 256 + 10, 75, 'Road length = ' + strmid(strtrim(string(road_length_tmp), 1), 0, 6) + ' km', color = 0, /device
; telescope
xyouts, 10, 60, 'Telescope', color = 0, /device
xyouts, 256 + 10, 60, 'Telescope', color = 0, /device
; diameter
xyouts, 10, 45, 'Diameter = ' + strmid(strtrim(string(diameter_def), 1), 0, 5) + ' m', color = 0, /device
xyouts, 256 + 10, 45, 'Diameter = ' + strmid(strtrim(string(diameter), 1), 0, 5) + ' m', color = 0, /device
; focal ratio
xyouts, 10, 30, 'Focal ratio = ' + strmid(strtrim(string(focal_ratio_def), 1), 0, 3), color = 0, /device
xyouts, 256 + 10, 30, 'Focal ratio = ' + strmid(strtrim(string(focal_ratio), 1), 0, 3), color = 0, /device
; pier height
xyouts, 10, 15, 'Pier height = ' + strmid(strtrim(string(pier_height_def), 1), 0, 4) + ' m', color = 0, /device
xyouts, 256 + 10, 15, 'Pier height = ' + strmid(strtrim(string(pier_height), 1), 0, 4) + ' m', color = 0, /device
; relative to the default observatory
xyouts, 256 + 150, 120, 'Relative to default', color = 0, /device 
xyouts, 256 + 150, 105, 'SNR = ' + strmid(strtrim(string(delta_snrs), 1), 0, 6), color = 0, /device 
xyouts, 256 + 150, 90, '(with AO ' + strmid(strtrim(string(delta_snrs_diffraction), 1), 0, 6) + ')', color = 0, /device 
xyouts, 256 + 150, 75, 'Cost = ' + strmid(strtrim(string(delta_costs), 1), 0, 6), color = 0, /device 
xyouts, 256 + 150, 60, '(with AO ' + strmid(strtrim(string(delta_costs_diffraction), 1), 0, 6) +')', color = 0, /device
xyouts, 256 + 150, 45, 'SNR/$ = ' + strmid(strtrim(string(delta_fom), 1), 0, 6), color = 0, /device 
xyouts, 256 + 150, 30, '(with AO ' + strmid(strtrim(string(delta_fom_diffraction), 1), 0, 6) + ')', color = 0, /device

; normalize signal-to-noise ratios by the values for the default telescope
snrs(0) = snrs(0)/snrs_def(0)
snrs(1) = snrs(1)/snrs_def(1)
snrs(2) = snrs(2)/snrs_def(2)
snrs_def(0) = snrs_def(0)/snrs_def(0)
snrs_def(1) = snrs_def(1)/snrs_def(1)
snrs_def(2) = snrs_def(2)/snrs_def(2)

; show the plot
wset, plots
if first eq 1 then begin ; look at the counter
 plot, costs(1:1), snrs(1:1), xrange = [1.e6, 0.5e11], /xlog, yrange = [1.e-5, 1.e5], /ylog, xtitle = 'Total observatory cost ($)', ytitle = 'Signal-to-noise ratio relative to default observatory', charsize = 1.25, psym = 4, thick = 3., symsize = 2.5, color = 0 ; natural seeing
endif else begin
 oplot, costs(1:1), snrs(1:1), psym = 4, thick = 3., symsize = 2.5, color = 0 ; natural seeing
endelse
oplot, costs(2:2), snrs(2:2), psym = 1, thick = 3., symsize = 2.5, color = 0 ; diffraction limited
oplot, costs_def(1:1), snrs_def(1:1), psym = 4, thick = 1., symsize = 2.5, color = 0 ; natural seeing
oplot, costs_def(2:2), snrs_def(2:2), psym = 1, thick = 1., symsize = 2.5, color = 0 ; diffraction limited

; show some further information with the plot
xyouts, 120, 512 - 15, 'Square = seeing limited, Plus = diffraction-limited adaptive optics', color = 0, /device
xyouts, 120, 512 - 30, 'Thick symbols = observatory, Thin symbols = default observatory ', color = 0, /device

end

;--------------------------------------
pro print_results

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost

common results, delta_costs, delta_snrs, delta_fom, delta_costs_diffraction, delta_snrs_diffraction, delta_fom_diffraction

common counter, first

; for a range of telescope diameter
for i = 0, 98 do begin
 diameter = 2. + float(i) ; meters

 ; set counter
 if i eq 0 then begin
  first = 1
 endif else begin
  first = 0
 endelse
 
 ; calculate 
 show_results

 ; print this out
; print, site, diameter, delta_costs, delta_snrs, delta_fom
 print, site, diameter, delta_costs_diffraction, delta_snrs_diffraction, delta_fom_diffraction

endfor

end

;--------------------------------------
pro world_observatory_v11

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost

common defaults, site_def, polar_cost_def, polar_background_def, polar_natural_def, polar_ground_def, altitude_def, road_length_def, $
                 telescope_def, diameter_def, focal_ratio_def, pier_height_def, wavelength_def, strehl_def, $
                 road_cost_def, enclosure_cost_def, telescope_cost_def, actuator_cost_def

common guardrails, polar_cost_min, polar_background_min, polar_natural_min, polar_ground_min, altitude_min, road_length_min, $
                   diameter_min, focal_ratio_min, pier_height_min, wavelength_min, strehl_min, $
                   road_cost_min, enclosure_cost_min, telescope_cost_min, actuator_cost_min, $
                   polar_cost_max, polar_background_max, polar_natural_max, polar_ground_max, altitude_max, road_length_max, $
                   diameter_max, focal_ratio_max, pier_height_max, wavelength_max, strehl_max, $
                   road_cost_max, enclosure_cost_max, telescope_cost_max, actuator_cost_max

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  pictures, plots

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
display = widget_base(/column, title = 'World Observatory')
output = widget_base(display, /row)
pictures = widget_draw(output, xsize = 512, ysize = 384)
plots = widget_draw(output, xsize = 384, ysize = 384)

; controls
controls = widget_base(display, /column)
controls_sliders = widget_base(controls, /row)
controls_buttons = widget_base(controls, /row)

; 'Default observatory'
controls_default_telescope = widget_base(controls_sliders, /column)
label = widget_label(controls_default_telescope, value = 'Default observatory', xsize = 150)
toggle_telescope = cw_bgroup(controls_default_telescope, ['DMT on Dome C', 'CFHT on Mauna Kea', 'Gemini on Mauna Kea', 'Gemini on Cerro Pachon', 'Keck on Mauna Kea', 'VLOT on Mauna Kea', 'CELT on Mauna Kea', 'Euro50 on La Palma', 'OWL on La Palma'], set_value = telescope_def, /exclusive, /column, uvalue='telescope')
;toggle_telescope = cw_bgroup(controls_default_telescope, ['CFHT on Mauna Kea', 'Gemini on Mauna Kea', 'Gemini on Cerro Pachon', 'Keck on Mauna Kea', 'VLOT on Mauna Kea', 'CELT on Mauna Kea', 'Euro50 on La Palma', 'OWL on La Palma'], set_value = telescope_def, /exclusive, /column, uvalue='telescope')
slider_strehl = cw_fslider(controls_default_telescope, maximum = strehl_max, minimum = strehl_min, uvalue = 'strehl', title = 'Strehl ratio', value = strehl_def, xsize = 150, drag = 1)

; 'Site'
controls_site = widget_base(controls_sliders, /column)
controls_site_top = widget_base(controls_site, /column)
label = widget_label(controls_site_top, value = 'Site', xsize = 250)
toggle_site = cw_bgroup(controls_site_top, ['Mid-latitude', 'Antarctic', 'Arctic'], set_value = site_def, /exclusive, /row, uvalue='site')
controls_site_top_first = widget_base(controls_site_top, /row) 
slider_altitude = cw_fslider(controls_site_top_first, maximum = altitude_max, minimum = altitude_min, uvalue = 'altitude', title = 'Altitude (m)', value = altitude_def, xsize = 125, drag = 1)
slider_road_length = cw_fslider(controls_site_top_first, maximum = road_length_max, minimum = road_length_min, uvalue = 'road_length', title = 'Road length (km)', value = road_length_def, xsize = 125, drag = 1)
label = widget_label(controls_site_top, value = 'Polar factors', xsize = 250)
controls_site_bottom = widget_base(controls_site, /row)
controls_site_bottom_first = widget_base(controls_site_bottom, /column)
slider_polar_cost = cw_fslider(controls_site_bottom_first, maximum = polar_cost_max, minimum = polar_cost_min, uvalue = 'polar_cost', title = 'Increased cost', value = polar_cost, xsize = 125, drag = 1)
slider_polar_background = cw_fslider(controls_site_bottom_first, maximum = polar_background_max, minimum = polar_background_min, uvalue = 'polar_background', title = 'Decreased background', value = polar_background, xsize = 125, drag = 1)
controls_site_bottom_second = widget_base(controls_site_bottom, /column)
slider_polar_natural = cw_fslider(controls_site_bottom_second, maximum = polar_natural_max, minimum = polar_natural_min, uvalue = 'polar_natural', title = 'Improved free seeing', value = polar_natural, xsize = 125, drag = 1)
slider_polar_ground = cw_fslider(controls_site_bottom_second, maximum = polar_ground_max, minimum = polar_ground_min, uvalue = 'polar_ground', title = 'Improved ground seeing', value = polar_ground, xsize = 125, drag = 1)

; 'Telescope'
controls_telescope = widget_base(controls_sliders, /column)
label = widget_label(controls_telescope, value = 'Telescope', xsize = 150)
slider_diameter = cw_fslider(controls_telescope, maximum = diameter_max, minimum = diameter_min, uvalue = 'diameter', title = 'Diameter (m)', value = diameter_def, xsize = 150, drag = 1)
slider_focal_ratio = cw_fslider(controls_telescope, maximum = focal_ratio_max, minimum = focal_ratio_min, uvalue = 'focal_ratio', title = 'Focal ratio', value = focal_ratio_def, xsize = 150, drag = 1)
slider_pier_height = cw_fslider(controls_telescope, maximum = pier_height_max, minimum = pier_height_min, uvalue = 'pier_height', title = 'Pier height (m)', value = pier_height_def, xsize = 150, drag = 1)
slider_wavelength = cw_fslider(controls_telescope, maximum = wavelength_max, minimum = wavelength_min, uvalue = 'wavelength', title = 'Wavelength (microns)', value = wavelength_def, xsize = 150, drag = 1)
;slider_strehl = cw_fslider(controls_telescope, maximum = strehl_max, minimum = strehl_min, uvalue = 'strehl', title = 'Strehl ratio', value = strehl_def, xsize = 150, drag = 1)

; 'Costs'
controls_costs = widget_base(controls_sliders, /column)
label = widget_label(controls_costs, value = 'Costs', xsize = 150)
slider_road_cost = cw_fslider(controls_costs, maximum = road_cost_max, minimum = road_cost_min, uvalue = 'road_cost', title = 'Road ($/km)', value = road_cost_def, xsize = 150, drag = 1)
;slider_enclosure_cost = cw_fslider(controls_costs, maximum = enclosure_cost_max, minimum = enclosure_cost_min, uvalue = 'enclosure_cost', title = 'Enclosure ($/m^3)', value = enclosure_cost_def, xsize = 150, drag = 1)
slider_enclosure_cost = cw_fslider(controls_costs, maximum = enclosure_cost_max, minimum = enclosure_cost_min, uvalue = 'enclosure_cost', title = 'Enclosure ($/m^2)', value = enclosure_cost_def, xsize = 150, drag = 1)
slider_telescope_cost = cw_fslider(controls_costs, maximum = telescope_cost_max, minimum = telescope_cost_min, uvalue = 'telescope_cost', title = 'Telescope ($/m^2)', value = telescope_cost_def, xsize = 150, drag = 1)
slider_actuator_cost = cw_fslider(controls_costs, maximum = actuator_cost_max, minimum = actuator_cost_min, uvalue = 'actuator_cost', title = 'Adaptive optics ($/dof)', value = actuator_cost_def, xsize = 150, drag = 1)

; buttons
buttons = widget_base(controls_sliders, /column)
button = cw_bgroup(buttons, ['Print results', 'Help', 'Reset all', 'About', 'Quit'], /column, xsize = 100, uvalue = 'buttons')

; realize the display and controls
widget_control, /realize, display
widget_control, /realize, controls
widget_control, pictures, get_value = pictures
widget_control, plots, get_value = plots

; show the default results
show_results

; submit the control widget to the xmanager
xmanager, 'world_observatory_v11_controls', controls

end

;--------------------------------------
pro world_observatory_v11_controls_event, event

common parameters, site, polar_cost, polar_background, polar_natural, polar_ground, altitude, road_length, $
                   telescope, diameter, focal_ratio, pier_height, wavelength, strehl, $
                   road_cost, enclosure_cost, telescope_cost, actuator_cost

common interface, toggle_site, slider_polar_cost, slider_polar_background, slider_polar_natural, slider_polar_ground, slider_altitude, slider_road_length, $
                  toggle_telescope, slider_diameter, slider_focal_ratio, slider_pier_height, slider_wavelength, slider_strehl, $
                  slider_road_cost, slider_enclosure_cost, slider_telescope_cost, slider_actuator_cost, $
                  pictures, plots

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
if uvalue eq 'telescope' then begin ; set a new telescope
 telescope = event.value
 set_defaults, 1
; ; this can be shut off in order to allow switch of default without change of observatory
; widget_control, toggle_site, set_value = site
; widget_control, slider_altitude, set_value = altitude
; widget_control, slider_road_length, set_value = road_length
; widget_control, slider_diameter, set_value = diameter
; widget_control, slider_focal_ratio, set_value = focal_ratio
; widget_control, slider_pier_height, set_value = pier_height
endif
if uvalue eq 'diameter' then diameter = event.value
if uvalue eq 'focal_ratio' then focal_ratio = event.value
if uvalue eq 'pier_height' then pier_height = event.value
if uvalue eq 'wavelength' then wavelength = event.value
if uvalue eq 'strehl' then strehl = event.value
if uvalue eq 'road_cost' then road_cost = event.value
if uvalue eq 'enclosure_cost' then enclosure_cost = event.value
if uvalue eq 'telescope_cost' then telescope_cost = event.value
if uvalue eq 'actuator_cost' then actuator_cost = event.value

; show the results
show_results

; action with the buttons
if uvalue eq 'buttons' then begin
 if event.value eq 0 then print_results
 if event.value eq 1 then xdisplayfile, 'world_observatory_v11.doc', title = 'World Observatory Help', group = event.top, height = 30, width = 75
 if event.value eq 2 then begin ; global reset of parameters
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
  show_results
 endif
 if event.value eq 3 then show_information
 if event.value eq 4 then widget_control, /reset
endif

end
