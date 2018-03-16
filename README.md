# pin
#### Now when someone says, "Let's put a pin it," you can.
 
Files can be boring. Ever so often, though, something stands out. When there's something significant in the file you're working with pin gives you a way of tracking and mangaging just the parts you care about.

pin is a utility which allows you to put pointers (pins) into text files. The pins are processed by the pin app and used to gather structured metadata about the file and the line the pin is on. The pins are stored seperately from the file itself allowing files to live where they need to, while the pins are kept together.

## Examples

### Placeholder

Suppose you're working on a project and realize the line you've just written is concerning/confusing/interesting in someway. You want to come back to that line, but you don't want to interupt your flow.

Instead of picking up a pen and scribling a note, switching to another application, or trying to remember the line you can drop a pin into the line (as a comment) and keep moving.

Like so:
``` haskell
type Foo {
     ...
}

bar :: Foo -> Baz
bar = <super cool code> // (>
```

Then, when you later remember there was that really neat thing on that one line you can run `pin scan`:

``` shell
~ pin scan <file>.hs
1 pin found. Enter an alias.
  File: <file>.hs
  Line: bar = <super cool code> // (>
```

The alias you gave to the pin is stored by pin along with the line itself so that you can review your handiwork.

``` shell
~ pin ls
bar (> <path>/<file>.hs
```

Any time you need to come back to your creation you can `show` it.

``` shell
~ pin show bar
bar = <super cool code> //
```

Note that for clarity the (> symbol (pronounced 'pin') is removed when the pin is displayed, but is still present in the file.

### Reminder

Later you're in a meeting, taking notes in a text editor because you never could get the hang of org-mode. Suddendly you're assigned a task that requires a little more thinking than you'd normally bother with.

Instead of switching to a to-do app, or later searching the file for the task in a sea of ill-concieved managerial thoughts, you can put a pin in the line of the file and continue transcribing your leader's tangental ramblings.

At the of the meeting you save your file and, as described above, you `scan` the file for pins, give them a aliases, and move on. 

When you're ready to start working on the task you list your existing pins.

``` shell
~ pin ls
donow (> ../meetingnotes.txt
todo1 (> ../notes.txt
todo (> ../projects/notes2.txt
review (> ../notes/asdf.txt
```

And you discover, unhappliy, that you've missed a couple of tasks that you forgot you'd previously pinned. To see how long ago it was that you added the pin to your list you use `detail`:

``` shell
~ pin detail donow
Alias: donow
    File: ../meetingnotes.txt
    Line: 10
    Pinned: 03/14/2017 05:04 PM
```

Thankfully the pin is so old it *must* be done. To remove the pin from your list you run:

``` shell
~ pin del donow
```

## More Commands

### alias
Change the alias of a pin.
``` shell
~ pin alias <current alias> <new alias>
```
### update
Refresh the hashes associated with a pin. (Usefull if you modify the file and pin yells at you.)
``` shell
~ pin update <pin alias>
```
### path
Change the path to the file that contains a pin.
``` shell
~ pin path <pin alias> <new file path>
```
### open (Experimental)
Open the file associated with a given pin.
``` shell
~ pin open <pin alias>
```

## Status

pin started based on a desire to the put the Haskell I'd learned on my own to practical use. As such I make no claims that any of the code in this tool represents the right way to Haskell. Much of what I've done was focused on making a working application, as opposed to using techniques or libraries that would be found in more polished Haskell projects.

This is a labor of, if not love, deep interest, but it is bounded by my personal and professional responsibilities. I will be updating this project as I'm able, but I make no claims about cadence of change or responsiveness to inquiries, issues, or requests.

### Caution

pin stores data in the *clear*. If you use it to keep track of anything financially or personally vital, you're insane.

## Roadmap

### Short Term
* better file parsing to detect pins not surrounded by spaces
* scan file should have flags to ignore or replace known pins
* bettter help text and `--help` for all commmands

### Medium Term
* output colors with import System.Console.ANSI
* displaying known pins should check the hash of the file and the line.
  if the pin has moved to a newline, but the hash has not changed, then just update the pin point.
* providing aliases for pins in the source file. perhaps like `(alias>`

#### New commands
* show related = shows all the pins in the file
* scan folder = like scan, but for all files in a folder
* scan folder recursive = above, but for all sub-folders
* scan here = scans the current directory with System.Directory.getCurrentDirectory
  * more sensible output for the above. perhaps lists with the first 40 chars of the line, the file, the path

#### Lists of pins
* list sort & filter = list all pins and apply basic organization
* list all = list all pins with more data
* list formatting = make the pins display fixed width

### Long Term
* some kind of settings to control file types, remove the pins on scan, ...
* remove the protopin from the line when creating new pin (controlled in settings)
* ability to add pin to any file
* ability to group pins

## Copyright

### Copyright
2018 Seth Dutcher

### License
GNU GPLv3

