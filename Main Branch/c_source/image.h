
#ifndef IMAGE_INCLUDED
#define IMAGE_INCLUDED

#include <assert.h>
#include <stdio.h>
#include "pixel.h"
#include "vector.h"


/**
 * constants
 **/
enum {
    IMAGE_SAMPLING_POINT,
    IMAGE_SAMPLING_BILINEAR,
    IMAGE_SAMPLING_GAUSSIAN,
    IMAGE_N_SAMPLING_METHODS
};

enum {
    IMAGE_CHANNEL_RED,
    IMAGE_CHANNEL_GREEN,
    IMAGE_CHANNEL_BLUE,
    IMAGE_CHANNEL_ALPHA,
    IMAGE_N_CHANNELS
};


/**
 * Line defined as two points (vectors).
 **/
struct Line
{
    Vector p, q;
};



/**
 * Image
 **/
class Image
{
public:
    Pixel *pixels;
    int width, height, num_pixels;
    int sampling_method;

public:
    // Creates a blank image with the given dimensions
    Image (int width, int height);

    // Copy iamage
    Image (const Image& src);

    // Destructor
    ~Image ();

    // Pixel access
    int ValidCoord (int x, int y)  { return x>=0 && x<width && y>=0 && y<height; }
    Pixel& GetPixel (int x, int y) { assert(ValidCoord(x,y)); return pixels[y*width + x]; }

    // Dimension access
    int Width     () { return width; }
    int Height    () { return height; }
    int NumPixels () { return num_pixels; }

    
#endif
