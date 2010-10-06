#include "Wrappers.h"

sfRenderWindow* wrp_sfRenderWindow_Create(sfVideoMode *mode, const char *title, unsigned long style, sfWindowSettings *params)
{
    sfRenderWindow_Create(*mode, title, style, *params);
}

void wrp_sfRenderWindow_GetSettings(sfRenderWindow *win, sfWindowSettings *settings)
{
    *settings = sfRenderWindow_GetSettings(win);
}

void wrp_sfRenderWindow_Clear(sfRenderWindow *win, sfColor *clr)
{
    sfRenderWindow_Clear(win, *clr);
}

sfEvent* sfEvent_Create()
{
    sfEvent *evt = malloc(sizeof(sfEvent));
    sfEvent voidEvt;
    *evt = voidEvt;
    return evt;
}

void wrp_sfVideoMode_GetDesktopMode(sfVideoMode *mode)
{
    *mode = sfVideoMode_GetDesktopMode();
}

void wrp_sfVideoMode_GetMode(size_t Index, sfVideoMode *mode)
{
    *mode = sfVideoMode_GetMode(Index);
}

sfBool wrp_sfVideoMode_IsValid(sfVideoMode* Mode)
{
    return sfVideoMode_IsValid(*Mode);
}

void wrp_sfColor_FromRGB(sfUint8 R, sfUint8 G, sfUint8 B, sfColor *clr)
{
    *clr = sfColor_FromRGB(R, G, B);
}

void wrp_sfColor_FromRGBA(sfUint8 R, sfUint8 G, sfUint8 B, sfUint8 A, sfColor *clr)
{
    *clr = sfColor_FromRGBA(R, G, B, A);
}

void wrp_sfColor_Add(sfColor *Color1, sfColor *Color2, sfColor *clr)
{
    *clr = sfColor_Add(*Color1, *Color2);
}

void wrp_sfColor_Modulate(sfColor *Color1, sfColor *Color2, sfColor *clr)
{
    *clr = sfColor_Modulate(*Color1, *Color2);
}

sfImage* wrp_sfImage_CreateFromColor(unsigned int Width, unsigned int Height, sfColor* Color)
{
    return sfImage_CreateFromColor(Width, Height, *Color);
}

void wrp_sfImage_Copy(sfImage *Image, sfImage *Source, unsigned int DestX, unsigned int DestY, sfIntRect *SourceRect)
{
    sfImage_Copy(Image, Source, DestX, DestY, *SourceRect);
}

sfBool wrp_sfImage_CopyScreen(sfImage *Image, sfRenderWindow *Window, sfIntRect *SourceRect)
{
    return sfImage_CopyScreen(Image, Window, *SourceRect);
}

void wrp_sfImage_CreateMaskFromColor(sfImage *Image, sfColor *ColorKey, sfUint8 Alpha)
{
    sfImage_CreateMaskFromColor(Image, *ColorKey, Alpha);
}

void wrp_sfImage_SetPixel(sfImage *Image, unsigned int X, unsigned int Y, sfColor *Color)
{
    sfImage_SetPixel(Image, X, Y, *Color);
}

void wrp_sfImage_GetPixel(sfImage *Image, unsigned int X, unsigned int Y, sfColor *clr)
{
    *clr = sfImage_GetPixel(Image, X, Y);
}

sfView* wrp_sfView_CreateFromRect(sfFloatRect *Rect)
{
    return sfView_CreateFromRect(*Rect);
}

void wrp_sfView_GetRect(sfView *View, sfFloatRect *Rect)
{
    *Rect = sfView_GetRect(View);
}

void wrp_sfSprite_SetColor(sfSprite *Sprite, sfColor *Color)
{
    sfSprite_SetColor(Sprite, *Color);
}

void wrp_sfSprite_GetColor(sfSprite *Sprite, sfColor *Color)
{
    *Color = sfSprite_GetColor(Sprite);
}

void wrp_sfSprite_SetSubRect(sfSprite *Sprite, sfIntRect *SubRect)
{
    sfSprite_SetSubRect(Sprite, *SubRect);
}

void wrp_sfSprite_GetSubRect(sfSprite *Sprite, sfIntRect *SubRect)
{
    *SubRect = sfSprite_GetSubRect(Sprite);
}

void wrp_sfSprite_GetPixel(sfSprite *Sprite, unsigned int X, unsigned int Y, sfColor *Pixel)
{
    *Pixel = sfSprite_GetPixel(Sprite, X, Y);
}

void wrp_sfShape_SetColor(sfShape *Shape, sfColor *Color)
{
    sfShape_SetColor(Shape, *Color);
}

void wrp_sfShape_GetColor(sfShape *Shape, sfColor *Color)
{
    *Color = sfShape_GetColor(Shape);
}

sfShape* wrp_sfShape_CreateLine(float P1X, float P1Y, float P2X, float P2Y, float Thickness, sfColor *Col, float Outline, sfColor *OutlineCol)
{
    return sfShape_CreateLine(P1X, P1Y, P2X, P2Y, Thickness, *Col, Outline, *OutlineCol);
}

sfShape* wrp_sfShape_CreateRectangle(float P1X, float P1Y, float P2X, float P2Y, sfColor *Col, float Outline, sfColor *OutlineCol)
{
    return sfShape_CreateRectangle(P1X, P1Y, P2X, P2Y, *Col, Outline, *OutlineCol);
}

sfShape* wrp_sfShape_CreateCircle(float X, float Y, float Radius, sfColor *Col, float Outline, sfColor *OutlineCol)
{
    return sfShape_CreateCircle(X, Y, Radius, *Col, Outline, *OutlineCol);
}

void wrp_sfShape_AddPoint(sfShape *Shape, float X, float Y, sfColor *Col, sfColor *OutlineCol)
{
    sfShape_AddPoint(Shape, X, Y, *Col, *OutlineCol);
}

void wrp_sfShape_GetPointColor(sfShape *Shape, unsigned int Index, sfColor *clr)
{
    *clr = sfShape_GetPointColor(Shape, Index);
}

void wrp_sfShape_GetPointOutlineColor(sfShape *Shape, unsigned int Index, sfColor *clr)
{
    *clr = sfShape_GetPointOutlineColor(Shape, Index);
}

void wrp_sfShape_SetPointColor(sfShape *Shape, unsigned int Index, sfColor *Color)
{
    sfShape_SetPointColor(Shape, Index, *Color);
}

void wrp_sfShape_SetPointOutlineColor(sfShape *Shape, unsigned int Index, sfColor *Color)
{
    sfShape_SetPointOutlineColor(Shape, Index, *Color);
}

void wrp_sfString_SetColor(sfString *Text, sfColor *Color)
{
    sfString_SetColor(Text, *Color);
}

void wrp_sfString_GetColor(sfString *Text, sfColor *Color)
{
    *Color = sfString_GetColor(Text);
}

void wrp_sfString_GetRect(sfString *View, sfFloatRect *Rect)
{
    *Rect = sfString_GetRect(View);
}

