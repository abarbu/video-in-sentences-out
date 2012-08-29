function frame = ffmpegGetFrame(video)

frame = permute(ffmpeg('get'),[2 1 3]);
