
std::string String_rep(int,std::string);
Image* getCropped(Image*,int,int,int,int);
void save(Image* ,char *);
void display(Image*);
using namespace cimg_library;
std::string String_rep(int n,std::string str)
{
 std::string temp;
 for(int i=0;i<n;i++)
 	temp +=str;
 return str;
}




Image* getCropped(Image *src,int rowl,int rowu,int coll,int colu)
{
	assert(rowl<=rowu);
	assert(coll<=colu);
	int w = colu-coll+1;
	int h =  rowu-rowl+1;
	int w2 = (*src).width;
    Image* crop = new Image(w,h);
	for(int i=0;i<h;i++)
	 	for(int j=0;j<w;j++)
	 		(*(crop)).pixels[i * w + j] = (*src).pixels[ (i+rowl) * w2 + (j+coll)];
	 return crop;
}



void save(Image* i,char *s)
{
FILE *F = fopen(s,"w");
BMPWriteImage(i,F);
fclose(F);
}


void display(Image* i)
{
save(i,"/home/vaibhav/plt_project/Vaibhav/temp.bmp");
CImg<unsigned char> image("/home/vaibhav/plt_project/Vaibhav/temp.bmp");
CImgDisplay main_disp(image,"Display");
while (!main_disp.is_closed()) 
	{
	main_disp.wait();
	}
}
