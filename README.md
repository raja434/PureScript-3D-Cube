# PureScript 3D Cube

**[Try it out here](http://georgevarghese185.github.io/PureScript-3D-Cube)**

This was my attempt at learning the basics of PureScript and functional programming for the first time by making a 3D rotating cube in the browser that the user can interact with.

## What it does
The PureScript code, once compiled, generates a JavaScript file. This file must be included in an HTML page (along with JQuery) and it draws a cube in the center of the page. The user can use their mouse to rotate the cube around it's 3 axes by clicking-and-dragging with their mouse.

Swiping the mouse quickly while clicking and dragging generates a velocity that is added to the cube so that the cube starts spinning. Swiping while the cube is already in motions just adds the new velocity to the current velocity (both magnitude and direction).

Clicking also generates friction on the cube and slows it down. The longer the mouse is held, the more the cube decelerates until standstill.

## Project Structure
The main directories here are
1. **src/**: Contains the PureScript source files. 

     * **Main.purs** is the entry point of the app. It draws the cube and handles mouse events and cube rotation. 
  
     * **Matrices.purs** contains some data structures and functions to deal with the transformation matrix (`matrix3d`) and rotation vector (`rotate3d`)  in CSS including converting them between a Matrix format and String form for the CSS `transform` property.
2. **docs/**: Contains the output of the code to be viewed in a browser.
    
     * **docs/index.html** can be accessed from the [GitHub pages link](http://georgevarghese185.github.io/PureScript-3D-Cube) of this project.
     
     * **docs/js/cube.js** contains the compiled js file obtained by compiling the PureScript code in **src/**.
     
     * (older commits)**docs/reference/**: I used this folder to place an HTML, CSS and JavaScript file for me to try out and test ideas before trying to implement them in PureScript.
3. **bower_components**/: I extended the Foreign Function Interface of the `Control.Monad.Eff.JQuery` module from the `purescript-jquery` package to include a function (`getCss`) to *get* CSS properties from a node since it only contained a function to *set* CSS properties.

## Global constants
There are some global constants in `Main` that can be changed to affect the cube behavior.
* `framesPerSecond`: Specifies the expected frames drawn per second by the browser. Used to calculated the required degrees/frame for cube rotation given the degrees/second.
* `speedSensitivity`: Specifies the interval at which the mouse position is polled in milliseconds. A lower value means the mouse must be idle for a longer period of time before letting go of the mouse button to be registered as a velocity of 0. Default value is 25 milliseconds.
* `rotationScale`: Multiplied by the mouse velocity of pixels/second to convert into degrees/sec. A value of 1.0 means 1 pixel/sec is taken as 1 degree/second. Default value is 0.4

* `decelRate`: Specifies the rate at which the cube should slow down from touch friction in degrees/frame<sup>2</sup> (divide degrees/sec<sup>2</sup> value by `framesPerSecond` to get degrees/frame<sup>2</sup>). Default value is 20 degrees/sec<sup>2</sup> (~0.33 degrees/frame<sup>2</sup>).

## Compiling
1. Compile the source code by using
    ```
    pulp build -O --to /path/to/output.js
    ```

2. Create a blank HTML file that includes at least 2 scripts: [JQuery](https://jquery.com/download/) and the output script from step 1.
3. Open the HTML file in a browser.
