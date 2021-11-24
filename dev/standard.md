
# Table of Contents

1.  [Keyword for GitHub Commit](#orgb257dad)
2.  [Github Commit Message](#org21df746)
3.  [Folders structure](#org74e00e4)
4.  [Naming style (function or object)](#org0c7940f)
5.  [Function names](#orgd055ad2)



<a id="orgb257dad"></a>

# Keyword for GitHub Commit

These keywords will be helpful when searching for relevant commits:

-   `Feat:` for new feature or function
-   `Fix:` for bug fix
-   `Doc:` for documentation or changes to documentation
-   `Style:` for changes to code or comments that aren&rsquo;t because of bugs
-   `Test:` for code testing in `tests` folder
-   `Misc:` for any changes that aren&rsquo;t critical or doesn&rsquo;t fit to any of the
    above keywords. Avoid using it if possible or only as a last resort
    
    Example of commit message
    
        Feat: Get list of new files
        
        New files are missing in the current list. They need to be in the
        current list before aggregating.


<a id="org21df746"></a>

# Github Commit Message

Use message for commit as follows:

    Keyword: Write summary as imperative or command
    
    More detail explanation if necessary with maximum 72 characters and use
    line break for long sentences.

The first line gives summary for what was done. Preferably to write this as
imperative mood or like commanding someone especially for commit related to
features, functions `(Feat:)` or fixing code `(Fix)`.


<a id="org74e00e4"></a>

# Folders structure

Where are the files located and how the files will be used.

-   Folder `/R` where all the R codes for the functions and objects for `orgdata`
    live.
-   Folder `/inst` is for SQL code files and other files that are needed by the package.
-   Folder `/dev` is for other files that aren&rsquo;t used in the package but good to
    have as reference or code testing.
-   Files with prefix `utils-*` in folder `/R` have helper functions that are used
    multiple times in different places.
-   Put the functions and helper functions in the same file if they aren&rsquo;t used in
    different places.


<a id="org0c7940f"></a>

# Naming style (function or object)

Use these naming style:

-   `snake_case` style is for **function** eg. `find_column_input`
-   `camelCase` style is for **object** eg. `fileGroup`
-   `kebab-case` style for **filename** eg. `utils-internal.R`


<a id="orgd055ad2"></a>

# Function names

An effort to group functions according to their responsibilities :)

-   Files with `read_` perfix for major functions.
-   Files with `do_` prefix for doing some action functions.
-   Files with `get_` prefix for column specific functions.
-   Files with `find_` prefix for helper functions.
-   Files with `is_` prefix for internal functions or utility to use `orgdata` package.
    
    Internal functions will not be exported but if needed, they can be accessed with
    `:::` eg. `orgdata:::is_separate()`.

