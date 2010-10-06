#include <SFML/Graphics.h>
#include <stdio.h>


void run(void* data)
{
    int n;
    for(n=0;;n++)
        printf("%d\n", n);
}

int main()
{
    sfThread* Thread = sfThread_Create(&run, NULL);    
    sfThread_Launch(Thread);

    sfWindowSettings Settings = {24, 8, 0};
    sfVideoMode Mode = {800, 600, 32};
    sfRenderWindow* App;
    sfImage* Image;
    sfSprite* Sprite;
    // sfString* Text;
    sfEvent Event;
    // float ft=0;

    /* Create the main window */
    App = sfRenderWindow_Create(Mode, "SFML window", sfClose, Settings);
    if (!App)
        return EXIT_FAILURE;

    sfRenderWindow_SetFramerateLimit(App, 60);

    /* Load a sprite to display */
    Image = sfImage_CreateFromFile("ankh-morpork.png");
    if (!Image)
        return EXIT_FAILURE;
    Sprite = sfSprite_Create();
    sfSprite_SetImage(Sprite, Image);

    /* Create a graphical string to display */
    // Text = sfString_Create();
    // sfString_SetText(Text, "--èàç--");

    /* Start the game loop */
    while (sfRenderWindow_IsOpened(App))
    {
        /* Process events */
        while (sfRenderWindow_GetEvent(App, &Event))
        {
            /* Close window : exit */
            if (Event.Type == sfEvtClosed)
                sfRenderWindow_Close(App);
            else if (Event.Type == sfEvtKeyPressed)
            {
                if (Event.Key.Code == sfKeyEscape)
                    sfRenderWindow_Close(App);
            }
        }

        /* Clear the screen */
        sfRenderWindow_Clear(App, sfBlack);

        /* Draw the sprite */
        sfRenderWindow_DrawSprite(App, Sprite);

        /* Draw the string */
        //sfRenderWindow_DrawString(App, Text);

        /* Update the window */
        sfRenderWindow_Display(App);

        // ft = sfRenderWindow_GetFrameTime(App);
        // if (ft != 0) printf("%f\n", 1/ft);
    }

    sfThread_Terminate(Thread);
    sfThread_Destroy(Thread);

    /* Cleanup resources */
    // sfString_Destroy(Text);
    sfSprite_Destroy(Sprite);
    sfImage_Destroy(Image);
    sfRenderWindow_Destroy(App);

    return EXIT_SUCCESS;
}

