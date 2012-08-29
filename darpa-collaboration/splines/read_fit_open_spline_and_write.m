function read_fit_open_spline_and_write(in,breaks,degree,samples,out);

D = importdata(in, ' ');
X = D(:,1)
Y = D(:,2)
pp = splinefit(X,Y,breaks,degree);
xx = linspace(min(X),max(X),samples);
y = ppval(pp,xx);
coefs = pp.coefs*1000;

fid = fopen([out '-spline.coefs'],'w');
fprintf(fid, '%d %d\n', int64(coefs));
fclose(fid);

fid = fopen([out '-spline.points'],'w');
fprintf(fid, '%d %d\n', int64([xx',y']));
fclose(fid);
