function I = ffmpegGetFrameN(video,n)

ffmpegOpenVideo(video);
frame = 1;
while(not(ffmpegIsFinished()))
    if (frame == n)
    I = ffmpegGetFrame();
    break
    end
    frame = ffmpegNextFrame();
end
ffmpegCloseVideo();
