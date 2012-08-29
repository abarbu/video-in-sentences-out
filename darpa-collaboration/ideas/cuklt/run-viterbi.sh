#!/bin/bash


# upgrade the video: Goes through each frame subdirectory and combine
# the .box data.... ???
#darpa-wrap ./upgrade-video -scmh 5000 -darpa HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2

# run the viterbi tracker with klt result from file (auto-select betweeen new
# and old format)
darpa-wrap ~/darpa-collaboration/ideas/x86_64-Linux-2.6.32-5-amd64/viterbi-tracker -scmh 6000 -t 12  -darpa HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2  /home/chang177/video-datasets/C-D1/voc4-models/ 5

# run the viterbi tracker with klt result from CUDA processor
darpa-wrap ~/darpa-collaboration/ideas/x86_64-Linux-2.6.32-5-amd64/viterbi-tracker -scmh 6000 -t 12 -cuda-klt -darpa HAVE6_A1_C2_Act8_1_URBAN_MC_AFTN_43a1e614-1dc6-11e0-ad1b-e80688ca39a2  /home/chang177/video-datasets/C-D1/voc4-models/ 5
