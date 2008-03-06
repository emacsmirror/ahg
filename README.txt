======================================================
 aHg: Alberto's Emacs front-end for the Mercurial SCM
======================================================

:Author: Alberto Griggio
:Contact: agriggio@users.sourceforge.net


DESCRIPTION
-----------

aHg is a simple Emacs front-end for the Mercurial (Hg) Distributed Source
Control Management (SCM) system. 

Its aims are simplicity and ease of use. It was inspired by DVC
(http://www.xsteve.at/prg/emacs_dvc/dvc.html), but it focuses exclusively on
Mercurial instead of supporting multiple Distributed SCMs. 

For a list of features, see the `QUICK GUIDE`_ section below.


DOWNLOAD
--------

An hg repository can be found at http://wxglade.sf.net/hg/ahg


INSTALLATION
------------

Put ``ahg.el`` in a directory where Emacs can find it (e.g. in
``/usr/share/emacs/site-lisp``). Alternatively, you can explicitly add the
directory of ``ahg.el`` to Emacs' ``load-path``, by adding this to your
``.emacs`` file::

  (setq load-path (cons "/dir/of/ahg/" load-path))

Then, simply add this to your ``.emacs``::

  (require 'ahg)


QUICK GUIDE
-----------

After the installation, an ``aHg`` menu appears as a child of the standard
``Tools`` menu of Emacs. The available commands are:

Status: 
   Shows the status of the current working directory, much like the
   ``cvs-examine`` command of PCL-CVS.

Log Summary:
   Shows a table with a short change history of the current working
   directory.

Detailed Log:
   Shows a more detailed change history of the current working directory.

Commit Current File:
   Commits the file you are currently visiting.

View Changes of Current File:
   Displays changes of current file wrt. the tip of the repository.

Execute Hg Command:
   Lets you execute an arbitrary hg command. The focus goes to the minibuffer,
   where you can enter the command to execute. You don't have to type ``hg``,
   as this is implicit. For example, to execute ``hg outgoing``, simply enter
   ``outgoing``. Pressing ``TAB`` completes the current command or file name.

Help on Hg Command:
   Shows help on a given hg command (again, use ``TAB`` to complete partial
   command names).
   

aHg buffers
~~~~~~~~~~~

The ``Status``, ``Log Summary`` and ``Detailed Log`` display their results on
special buffers. Each of these has its own menu, with further available
commands.



LICENSE
-------

The program is released under the `GNU GPL v3`__ License.

__ http://www.gnu.org/copyleft/gpl.html
