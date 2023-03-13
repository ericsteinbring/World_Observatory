World Observatory
Eric Steinbring
V1.0 29 November 2004
V2.0 2 June 2005
V1.1 10 December 2005
V2.1 10 December 2005

Unzip and extract the tar-files.

You should have the following files (x is the edit number):

hdfs_nicmos.fits     spectrum_lines.dat        world_observatory_v2x.doc
make_data.pro        spectrum_sky.dat          world_observatory_v2x.pro
ReadMe.txt           world_observatory_v1x.doc
spectrum_galaxy.dat  world_observatory_v1x.pro

You must have the IDL astrolib libraries installed.
See http://idlastro.gsfc.nasa.gov/.

The specific utilities needed are:
READFITS
WRITEFITS
CONVOLVE
PSF_GAUSSIAN
GAUSSIAN

Launch IDL.

There are two versions of the code. V1 is the initial point-source simulator.
V2 deals with galaxies.

Compile the V1 code by entering:
>.compile world_observatory_v1x

Run it by entering:
>world_observatory_v1x

To run V2.0 you must first run

>.r make_data.pro

This generates the input files necessary to run V2. These are:

background_spectrum.dat
perfect_bulge.fits
perfect_disk.fits
perfect_spectrum.fits

You need only run it once at install, but it can be used to alter the 
galaxy disk component going into the simulation.

Compile the code by entering:
>.compile world_observatory_v2x

Run it by entering:
>world_observatory_v2x

The galaxy survey output is written to results.dat. The comparison
of observatories output is written to table.dat.

See the help files for more information.
