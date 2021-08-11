# rPACI

  This Github repository is devoted to the development of the R package **rPACI** (R Placido Analysis of Corneal Irregularity), also available in the [the CRAN repository](https://cran.r-project.org/package=rPACI).

  ## Contributors
  The contributors/developers of this project are:
  * Darío Ramos-López,  Rey Juan Carlos University, Spain.  dario.ramos.lopez at urjc.es
  * Ana D. Maldonado, University of Almería, Spain. ana.d.maldonado at ual.es
  
  To contribute to the package, report a bug, or any other issues, please use our [GitHub repository rPACI](https://github.com/dariorlual/rPACI).

  ## rPACI aim and description
  
  This package performs analyses of corneal data obtained from a Placido disk corneal topographer, with the calculation of the Placido irregularity indices and the posterior analysis. The package is intended to be easy to use by a practitioner, providing a simple interface and yielding easily interpretable results.
  
  A corneal topographer is an ophthalmic clinical device that obtains measurements in the cornea (the anterior part of the eye). A Placido disk corneal topographer makes use of the Placido disk [Rowsey et al. (1981)](https://doi.org/10.1001/archopht.1981.03930011093022), which produce a circular pattern of measurement nodes. The raw information measured by such a topographer is used by practitioners to analyze curvatures, to study optical aberrations, or to diagnose specific conditions of the eye (e.g. keratoconus, an important corneal disease). 
  
  The rPACI package allows the calculation of the corneal irregularity indices described in [Castro-Luna et al. (2020)](https://doi.org/10.1016%2Fj.clae.2019.12.006), [Ramos-Lopez et al. (2013)](https://doi.org/10.1097%2FOPX.0b013e3182843f2a), and [Ramos-Lopez et al. (2011)](https://doi.org/10.1097/opx.0b013e3182279ff8). It provides a simple interface to read corneal topography data files as exported by a typical Placido disk topographer, to compute the irregularity indices mentioned before, and to display summary plots that are easy to interpret for a clinician.

  **We encourage the users to get started by reading the package's vignettes. We recommend beginning with 'Workflow of the rPACI package' and then consulting the other vignettes for additional details.**
  
  ## Corneal topography
  
  The cornea is the external (outermost) part of the eye, and it is responsible for most of the eye's refractive power. Given its importance in vision, its analysis is especially important in clinical ophthalmology, since it can be affected by several diseases (e.g., keratoconus). Besides, refractive surgery to correct vision defects acts on the cornea, changing its curvature. Thus, the correct analysis and diagnosis of the cornea are crucial for practitioners. 
  
  Corneal topography refers to the measurement of the corneal surface shape using an automated device, which is called a corneal topographer. Most corneal topographers used in clinical practice rely on the Placido disk technology (either alone or in combination with others, such as the Scheimpflug camera).

  For a deeper insight about these and other related topics (corneal topography, Placido disk technology, keratoconus, ...), we refer the reader to the rPACI's vignette entitled 'Corneal topographers and data formats', or to the websites by [University of Iowa Health Care](https://webeye.ophth.uiowa.edu/eyeforum/tutorials/Corneal-Imaging/Index.htm) and [American Academy of Ophthalmology](https://eyewiki.aao.org/Corneal_Topography).
  
  ## rPACI's file formats
  
  The **rPACI** package is able to read corneal topography files in two different formats, by now. It will possibly be expanded in the future allowing formats used by other manufacturers. Both formats rely on plain text files. The first file format is basically the one employed by [CSO](https://www.csoitalia.it) topographers, but with some more flexibility. The second file format follows the structure used internally by **rPACI**. It has been developed to manage directly the datasets handled by the package, allowing to save and read data easily. These two formats are described in the next subsections.
  
  You can find more details about supported formats in the vignette 'Corneal topographers and data formats'.	
  
  ### File format by CSO topographers
	
  A corneal topography file exported by CSO topographers can be read directly in **rPACI**. The raw measurement file should have the following structure:

  * A header (of any length, possibly missing) with meta-data: patient data, optical measurements, date of exploration, etc. Its size depends on the device. For CSO topographers, the header typically has 24 lines. The function used in **rPACI** to read the files, `readCSO`, is able to detect the header lines (those that do not contain only real numbers) and to skip them.
  * A list of size N_R x N_A with the rho coordinates (distance to the origin in polar coordinates, measured in mm) of the digitized points, at equally spaced angles (theta, the argument in polar coordinates). The theta coordinate is inferred from the position (assuming a uniform distribution), as it is not explicitly given in the data file.
  * A list of size N_R x N_A with the z coordinate, which can be altitude (elevation) or curvature, depending on the topographer and the exportation settings. These data are post-processed by the device using different algorithms, not directly measured, and the z values are not used in **rPACI**.
	
  ### File format by rPACI
	
  **rPACI** also reads another format of corneal topography files. This format was designed to follow the internal datasets' structure. A file formatted in this manner is obtained when using the writing functions of the package. It should have the following structure:	
		
  * A header (of any length, possibly missing) with meta-data. In case that the saved dataset was obtained by simulation, the header includes the list of simulation parameters. Else (if the dataset was originally read from a real measurement file in other formats), no header will be present.
  * A block of three separated columns (with some specified character separator), according to the usual format used by **rPACI**, i.e., a list with three columns (x and y coordinates of each point, and its ring index) and a row per data point.
  
  ## Placido irregularity indices
  
  The **rPACI** package is able to read corneal topography files and then analyze them. This analysis includes the computation of some Placido irregularity indices. The indices computed by **rPACI** allow to discriminate between normal and irregular corneas. Most of them were introduced and validated with real datasets in the scientific publications [Ramos-Lopez et al. (2013)](https://doi.org/10.1097%2FOPX.0b013e3182843f2a), and [Ramos-Lopez et al. (2011)](https://doi.org/10.1097/opx.0b013e3182279ff8). An additional index, based on a naive Bayes classifier, was proposed later, and tested with a different database, in [Castro-Luna et al. (2020)](https://doi.org/10.1016%2Fj.clae.2019.12.006). In these papers, all indices demonstrated a good sensitivity for the detection of keratoconus, a corneal disease.
		
  Consult more information about the Placido irregularity indices in the vignette 'Mathematical definition of the Placido irregularity indices'.	
		
  ## Analyzing datasets 
  
  The package includes several easy-to-use functions that allow analyzing data from a single file or dataset, or from a folder (with several files either corresponding to the same or to different patients). All these functions include easily interpretable plots and yield well-formatted data.frames to facilitate the posterior analysis of results.
		
  You can find more details about supported formats in the vignette 'Workflow of the rPACI package'.	
