##############################
TrophyFetcher |license| |pypi|
##############################

.. |license| image:: https://img.shields.io/pypi/l/trophyfetcher.svg?style=flat-square
    :target: https://github.com/robertomlsoares/trophyfetcher/blob/master/LICENSE.txt

.. |pypi| image:: https://img.shields.io/pypi/v/trophyfetcher.svg?style=flat-square
    :target: https://pypi.python.org/pypi/trophyfetcher

``TrophyFetcher`` provides a way to extract information from the `PSN Public Trophies`_ website.
It uses ``Selenium`` and ``PhantomJS`` to access the website and scrape it, therefore, there are
private information that ``TrophyFetcher`` is not able to extract (everything that is not on the website).

.. _PSN Public Trophies: https://www.playstation.com/en-us/my/public-trophies/

Installation
============

You need to install ``PhantomJS`` to be able to use ``TrophyFetcher``.
There are multiple ways to install it, such as with ``npm``.

.. code-block:: console

    $ npm install -g phantomjs

If you do not have ``npm`` installed and do not want to install it, I recommend checking their `website`_ to
look for other alternatives.

.. _website: http://phantomjs.org/

To install ``TrophyFetcher`` itself, simply do:

.. code-block:: console

    $ pip install trophyfetcher

Remember that you need ``Selenium`` as well, but ``pip`` will most likely that care of that for you.

Tutorial
========

All you need to do is create a ``TrophyFetcher`` object and call the ``user_info`` method passing
the username of the user you want to extract information from.

.. code-block:: python

    from trophyfetcher import TrophyFetcher

    tf = TrophyFetcher()
    extracted_info = tf.user_info('username')

``TrophyFetcher`` will automatically log errors in a file called ``trophyfetcher_log.txt``, but you
can turn that off by doing:

.. code-block:: python

    from trophyfetcher import TrophyFetcher

    tf = TrophyFetcher()
    tf.logging_option(False)
    extracted_info = tf.user_info('username')

If a problem occurs and ``Selenium`` is not able to locate elements on the page, ``TrophyFetcher`` will
raise a ``TimeoutException`` and log (if logging is enabled) which element caused the error.
If that happens, it is most likely due to slow connection or a problem with the website, since
``TrophyFetcher`` will wait until 30 seconds before it raises the ``TimeoutException``.

Finally, if everything goes well, you will receive an object in the following format (JSON):

.. code-block:: javascript

    {
        "avatar_url": "https://avatar_example.png",
        "bronze_trophies": "0",
        "games_info": [{
            "bronze_trophies": "0",
            "gold_trophies": "0",
            "img_url": "https://game_example.png",
            "name": "example",
            "platform": "example platform",
            "platinum_trophies": "0",
            "progress": "0%",
            "silver_trophies": "0"
        }],
        "gold_trophies": "0",
        "level": "0",
        "level_progress": "0",
        "platinum_trophies": "0",
        "silver_trophies": "0",
        "trophies": "0"
    }
