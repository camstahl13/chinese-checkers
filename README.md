# Chinese Checkers
## Basic Description
Chinese Checkers implemented in Racket. Supports 2, 3, 4, or 6 player gameplay. Needs additional win scenario-handling logic.
## Instructions
### General Chinese Checkers Instructions
Here's a nice resource: https://www.mastersofgames.com/rules/chinese-checkers-rules.htm
### Menu Navigation Instructions
Use the left and right arrow keys to navigate the menu options for number of players. Use space to select number of players. Press enter to begin the game.
### Gameplay Instructions
Use the arrow keys to move your cursor. Left and right arrow keys rotate the cursor counter-clockwise and clockwise, respectively. Up and down arrow keys move the cursor forward and backward, respectively. Press space to select whatever piece is under your cursor. Once a piece is selected, you can either move that piece to a valid slot (by positioning the cursor over that slot and pressing space) or unselect the piece and start your move afresh (by pressing escape). Your turn will not come to an end as soon as you have moved a piece. Rather, you will be given the opportunity to continue moving your piece among valid destinations. Keep in mind that you cannot combine jumps (e.g. make a triple-jump with one movement of the selected piece) or jump onto a different "path" (a "path" being a series of consecutive, valid jumps beginning at a piece's starting position) without first returning to the starting position (either by using escape or by returning manually). Hitting enter will solidify one's move and end one's turn; of course, doing so is allowed only when a piece is not at its starting position.
