
----------------
How to compile
----------------
cmake ./
make

------------
How to run
------------
o per-video-klt example:

   cd bin
   ./per-video-klt ~chang177/video-datasets/klt_desk_demo.avi

o per-frame-klt example:

   cd bin
   ./per-frame-klt file1.jpg file2.jpg

o desk data example (for debugging):

   cd bin
   ./klt_tracker -f data_desk_scene.cfg


------------------
Notes and Todos:
------------------
Watch the memory usage...  

-------------------------
Source code information
-------------------------
Website:
  http://www.cs.cmu.edu/~myung/IMU_KLT/KLT_gpu_cuda.html

Source Code from: 
    http://www.cs.cmu.edu/~myung/IMU_KLT/klt_tracker_v1.0.zip

Data from: 
    http://www.cs.cmu.edu/~myung/IMU_KLT/data_desk_scene.zip


---------------------------
Code modifications
---------------------------
Modification Summary:
   - Remove IPP dependency.
   - Skip smoothing step.
   - Skip camera model.
   - Allow image dimension up to 1280x1280

File modified:
     CMakeLists.txt
     src/main_tracker.cxx
     src/playback_tracker.cxx
     src/CMakeLists.txt
     src/imageAlign.cxx
     src/cuda_image_align.cu
     src/featurePool.cxx
     src/featurePool.h
     src/featureTrack.h
     src/cuda_feature_select.cxx
     data/desk_img.log
 
Files removed:
     FindIPP.cmake

Files added:
     README.txt
     src/per-frame-klt.cxx
     src/per-video-klt.cxx



