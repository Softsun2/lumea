# Lumea
Hacky webservice using [Pandoc](https://pandoc.org/) to convert markup formats to static html. Useful for writing static websites in your favorite markup formats such as org or markdown.

## Build

## Run

## notes

More like a blog template.

Want to convert any highlevel markdown format to html, have a super tiny, easy-to-make, extendable static blog.

### Architecture Options
- Blog as file heirarchy
- Or the less invasive option is to recursively follow links and then
  flatten them when converting. I.e., the html directory will be flat
  - Though you would have to avoid collisions (hash or filepath)
  - Is removing and creating directories really that bad?
- Or, don't even write the files, convert and serve them to order with caching

### Blog Post Meta-Data
- [ ] Created (date)
- [ ] Edited

### Storyboard
1. Author updates highlevel markup format
2. Author signals a site update when ready
    - i.e., a command
3. Server converts plaintext to html
    - using Pandoc
        - can't depend on pandoc's yaml meta data (supported for a narrow subset
          of markup formats)
    - conversion shouldn't be risky for the server!
    - I've decided to hack together a crummy minimal webserver
        - an exercise in performance, security, and error handling
    - ideally only convert updated files
        - options: make, git, file-meta data, db
4. Updated content may be served
    - should defer requests while files are being built
5. Server waits for process to repeat (accpeting requests as usual)

### Functionality
- How to write a nav module?
- Filter posts by meta data

## Tasks

- [ ] design generation structure for src and dest static files
    - description: a design needs to be chosen so a user's markup files are
      generated into html that can be served, design should prioritize security
      (minimize & localize IO) and usability.
    - notes: options:
        * flattened: pros: don't need to make/remove directories. cons: name 
          collisions, though with hashing by byte content would be rather
          effective
        * **mirror**: pros: heirarchy, easy to translate links. cons: riskier to
          give server access to file system. maybe there's an idea of "read
          only" where the program can only delete files it's created. or just be
          careful, attempt to prove the program has safe file IO, test it like
          crazy
        * baked, single html file: cons: sounds like a debugging nightmare
        * **translate files to serve**: pros: minimal file IO, slower. cons: slower,
          doesn't scale, though I'd be interested in seeing its performance
    - status: done: use mirror approach, hard code paths to the site (src :
    markup :: dest : html) so users don't shoot themselves in the foot

- [ ] implement generation structure
    - description: implementation of mirror translation structure in haskell using
      pandoc
    - status: todo

- [ ] investigate translating only "dirty" files
    - description: only want to translate files that need to be translated, some
      leads: make, git, cross reference already translated files
    - status: todo

- [ ] implement translating only "dirty" files
    - description: update when investigation complete
    - status: todo

- [ ] define list of required meta data
    - description: define what subset of meta data is required to:
        * sort posts by: date, tag/category
        * only regenerate updated markup files
        * logging: created, edited, deleted (internal)
    - status: todo

- [ ] minimal get only http server
    - description: implement a minimal web server in haskell that only asks
      reasonable get requests, server must support at least a few multiple
      connections
    - status: ready for testing / refactorable

- [ ] investigate usability
    - description: answer the following:
        * are global blocks (e.g navbars) feasible?
            * I believe so, markup is semantic/heirachical, would be able to
              insert header blocks, though a simple implementation of this
              would be crucial
        * sane templating?
            * Again technically yes, though a simple implementation sounds
              difficult
            * would require users to choose where their content is inserted into
              their markup using only markup... And would ideally require as
              little programming knowledge as possible
    - status: thought about it, not required to implement, something to comeback
      to when appropriate
