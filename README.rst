.. image:: http://melpa.org/packages/pungi-badge.svg
           :target: MELPA_
================================================================================
pungi - A Python development tool to Integrate Jedi with virtualenv and buildout
================================================================================
The primary purpose of this package is to integrate the Emacs24 builtin python-mode_
package with jedi_ for ease of developing Python code with Emacs_.

When working within a virtualenv, ``pungi:setup-jedi`` configures the
``python`` modes' ``python-shell-virtualenv-path`` to be the current
virtualenv, and when a buildout is detected via the presence of an
``omelette`` part, sets ``python-shell-extra-pythonpaths`` , which
enables jedi commands ``jedi:complete``, ``jedi:goto-definition`` and
``jedi:doc`` operate on the correct source files.

``pungi`` was originally inspired by the following gist:

    https://gist.github.com/nyergler/6100112

For the curious, the name ``pungi`` is the Indian name for a ``snake charmer``.

Installation
============
Ensure that the name "emacs" in your shell points to the Emacs
executable. i.e On $PATH or aliased.

This package can be installed via the ``list-packages`` interface in Emacs.

If not using ELPA (i.e list-packages), then add the following to
you init.el/.emacs:

(add-to-list 'load-path 'path-to-this-file)

Using ELPA, i.e when ``pungi`` is installed via ``list-packages``:

.. code-block: lisp

   (require 'pungi)


In all cases, call ``pungi:setup-jedi`` within a ``python-mode-hook``

.. code-block: lisp

   (add-hook #'python-mode-hook '(lambda () (pungi:setup-jedi)))


Manual testing:

When visiting a python buffer, move the cursor over a symbol and check
that invoking ``M-x jedi:goto-definition`` (Usually bound to the
key ``M-.``) opens a new buffer showing the source of that python
symbol.


.. _Emacs: https://www.gnu.org/software/emacs/
.. _Plone: http://www.plone.org
.. _Zope: http://www.zope.org
.. _buildout: http://www.buildout.org/en/latest/
.. _jedi: http://jedi.jedidjah.ch/en/latest/
.. _python-mode: https://github.com/fgallina/python.el
.. _virtualenv: https://virtualenv.pypa.io/en/latest/
.. _MELPA: http://melpa.org/#/pungi
