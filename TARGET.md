# facsimile next targets

The dream for facsimile is to rival gui editors in terms of featureset
So the full vision for fac involves:
- split panes
- terminal entry binds
- tabs
- better conceptualization of having a "folder open", and better conceptualization of a workspace
- toggleable interactive file tree (my `fuss` implementation so that git is integrated in the editor)
  - that is a 30% pane that has my fuss program show the tree with fuss binds supported

I think the first order of business is some kind of scaffolding to support this workspace-folder paradigm
and then the targets in order will be:
-  the fuss file explorer
- tabs for editing
- integrating tabs and fuss binds
- split pane mode
- thinking about how to incorporate execution of in progress files, basically bring up an integrated terminal