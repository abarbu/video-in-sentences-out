function ffmpegExpandVideo(video)

ffmpegOpenVideo(video);
frame = 1;
while(not(ffmpegIsFinished()))
    imwrite(ffmpegGetFrame(),sprintf('frame-%04d.ppm',frame));
    sprintf('frame-%04d.ppm',frame)
    frame = ffmpegNextFrame();
end
ffmpegCloseVideo();
