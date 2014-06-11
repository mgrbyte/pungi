=================================================================================
pungi - A Python developement tool to Integrate Jedi with virtualenv and buildout
=================================================================================

The primary purpose of this package is to integrate the Emacs24 builtin python-mode_
package with jedi_ for ease of developing Python code with Emacs_.

The primary features provided are:

 * Integration with jedi_, in particular enabling jedi:goto-definition to
   work with the Python environment.

 * Integration with buildout_ based projects (e.g in Plone_, Zope_)

 * Integration with virtualenv_

These features are adapted from the following gist:

    https://gist.github.com/nyergler/6100112

For the curious, ``pungi`` is the Indian name for a ``snake charmer``.

Installation
============
Ensure that the name "emacs" in your shell points to the Emacs
executable. i.e On $PATH or aliased.

This package can be installed via the ``list-packages`` interface in Emacs.

.. _Emacs: https://www.gnu.org/software/emacs/
.. _Plone: http://www.plone.org
.. _Zope: http://www.zope.org
.. _buildout: http://www.buildout.org/en/latest/
.. _jedi: http://jedi.jedidjah.ch/en/latest/
.. _python-mode: https://github.com/fgallina/python.el
.. _virtualenv: https://virtualenv.pypa.io/en/latest/
