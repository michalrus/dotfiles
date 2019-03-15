{
  "selectedHostsFiles": [
    "malware-0",
    "malware-1",
    "dpollock-0",
    "hphosts",
    "mvps-0",
    "plowe-0"
  ],
  "selectedRecipeFiles": [
    "recipes_en-0"
  ],
  "liveHostsFiles": [
    [
      "malware-0",
      {
        "content": "filters",
        "title": "Malware Domain List",
        "contentURL": [
          "https://www.malwaredomainlist.com/hostslist/hosts.txt",
          "assets/thirdparties/www.malwaredomainlist.com/hostslist/hosts.txt"
        ],
        "type": "filters",
        "hasLocalURL": true,
        "hasRemoteURL": true,
        "updateAfter": 13,
        "cached": true,
        "writeTime": 1550487034500,
        "obsolete": false,
        "remoteURL": "https://www.malwaredomainlist.com/hostslist/hosts.txt",
        "entryCount": 1109,
        "entryUsedCount": 1109,
        "selected": true
      }
    ],
    [
      "malware-1",
      {
        "content": "filters",
        "title": "Malware domains",
        "contentURL": [
          "https://mirror.cedia.org.ec/malwaredomains/justdomains",
          "https://mirror1.malwaredomains.com/files/justdomains",
          "assets/thirdparties/mirror1.malwaredomains.com/files/justdomains",
          "assets/thirdparties/mirror1.malwaredomains.com/files/justdomains.txt"
        ],
        "supportURL": "http://www.malwaredomains.com/",
        "type": "filters",
        "hasLocalURL": true,
        "hasRemoteURL": true,
        "updateAfter": 13,
        "cached": true,
        "writeTime": 1550487160200,
        "obsolete": false,
        "remoteURL": "https://mirror.cedia.org.ec/malwaredomains/justdomains",
        "entryCount": 26882,
        "entryUsedCount": 26867,
        "selected": true
      }
    ],
    [
      "dpollock-0",
      {
        "content": "filters",
        "updateAfter": 11,
        "title": "Dan Pollock’s hosts file",
        "contentURL": [
          "https://someonewhocares.org/hosts/hosts",
          "assets/thirdparties/someonewhocares.org/hosts/hosts.txt"
        ],
        "supportURL": "https://someonewhocares.org/hosts/",
        "type": "filters",
        "hasLocalURL": true,
        "hasRemoteURL": true,
        "cached": true,
        "writeTime": 1550487284500,
        "obsolete": false,
        "remoteURL": "https://someonewhocares.org/hosts/hosts",
        "entryCount": 14135,
        "entryUsedCount": 14117,
        "selected": true
      }
    ],
    [
      "hphosts",
      {
        "content": "filters",
        "updateAfter": 11,
        "title": "hpHosts’ Ad and tracking servers",
        "contentURL": [
          "https://hosts-file.net/.%5Cad_servers.txt",
          "assets/thirdparties/hosts-file.net/ad_servers.txt"
        ],
        "supportURL": "https://hosts-file.net/",
        "type": "filters",
        "hasLocalURL": true,
        "hasRemoteURL": true,
        "cached": true,
        "writeTime": 1550487407100,
        "obsolete": false,
        "remoteURL": "https://hosts-file.net/.%5Cad_servers.txt",
        "entryCount": 45745,
        "entryUsedCount": 43078,
        "selected": true
      }
    ],
    [
      "mvps-0",
      {
        "content": "filters",
        "updateAfter": 11,
        "title": "MVPS HOSTS",
        "contentURL": [
          "http://winhelp2002.mvps.org/hosts.txt",
          "assets/thirdparties/winhelp2002.mvps.org/hosts.txt"
        ],
        "supportURL": "http://winhelp2002.mvps.org/",
        "type": "filters",
        "hasLocalURL": true,
        "hasRemoteURL": true,
        "cached": true,
        "writeTime": 1550487529100,
        "obsolete": false,
        "remoteURL": "http://winhelp2002.mvps.org/hosts.txt",
        "entryCount": 12228,
        "entryUsedCount": 8386,
        "selected": true
      }
    ],
    [
      "plowe-0",
      {
        "content": "filters",
        "updateAfter": 13,
        "title": "Peter Lowe’s Ad and tracking server list",
        "contentURL": [
          "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=1&mimetype=plaintext",
          "assets/thirdparties/pgl.yoyo.org/as/serverlist",
          "assets/thirdparties/pgl.yoyo.org/as/serverlist.txt"
        ],
        "supportURL": "https://pgl.yoyo.org/adservers/",
        "type": "filters",
        "hasLocalURL": true,
        "hasRemoteURL": true,
        "cached": true,
        "writeTime": 1550487650000,
        "obsolete": false,
        "remoteURL": "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=1&mimetype=plaintext",
        "entryCount": 2965,
        "entryUsedCount": 1601,
        "selected": true
      }
    ]
  ],
  "userMatrix": [
    "https-strict: behind-the-scene false",
    "referrer-spoof: behind-the-scene false",
    "https-strict: * true",
    "referrer-spoof: * true",
    "noscript-spoof: * true",

    "matrix-off: about-scheme true",
    "matrix-off: chrome-extension-scheme true",
    "matrix-off: chrome-scheme true",
    "matrix-off: moz-extension-scheme true",
    "matrix-off: opera-scheme true",
    "matrix-off: vivaldi-scheme true",
    "matrix-off: wyciwyg-scheme true",
    "matrix-off: behind-the-scene true",

    "* * * block",
    "* * css allow",
    "* * image allow",
    "* * frame block",
    "* 1st-party * allow",
    "* 1st-party frame allow",

    "matrix-off: binance.com true",

    "* cdnjs.cloudflare.com script allow",
    "allegro.pl allegrostatic.com script allow",
    "allegro.pl assets.allegrostatic.com script allow",
    "discordapp.com discord.gg * allow",
    "discordapp.com discordapp.net * allow",
    "ebay.com ebayrtm.com script allow",
    "ebay.com ebaystatic.com script allow",
    "facebook.com fbcdn.net script allow",
    "facebook.com fbcdn.net xhr allow",
    "filmweb.pl fwcdn.pl script allow",
    "github.com github-production-repository-file-5c1aeb.s3.amazonaws.com xhr allow",
    "github.com github-production-user-asset-6210df.s3.amazonaws.com xhr allow",
    "github.com githubapp.com * allow",
    "github.com githubassets.com script allow",
    "github.com githubusercontent.com * allow",
    "github.com render.githubusercontent.com frame allow",
    "google.com gstatic.com * allow",
    "google.com gstatic.com script allow",
    "google.com gstatic.com xhr allow",
    "google.com maps.googleapis.com * allow",
    "google.com www.gstatic.com xhr allow",
    "hubstaff.com cloudfront.net script allow",
    "instagram.com cdninstagram.com media allow",
    "leafly.com d3ix816x6wuc0d.cloudfront.net script allow",
    "mail.google.com content.googleapis.com xhr allow",
    "mail.google.com ggpht.com xhr allow",
    "mail.google.com gstatic.com * allow",
    "mail.google.com mail-attachment.googleusercontent.com frame allow",
    "messenger.com cdn.fbsbx.com media allow",
    "messenger.com facebook.com cookie allow",
    "messenger.com facebook.com xhr allow",
    "messenger.com facebook.net script allow",
    "messenger.com fbcdn.net media allow",
    "messenger.com fbcdn.net script allow",
    "messenger.com fbcdn.net xhr allow",
    "michalrus.com apis.google.com script allow",
    "michalrus.com calendar.google.com frame allow",
    "michalrus.com calendar.google.com other allow",
    "michalrus.com calendar.google.com script allow",
    "michalrus.com cdnjs.cloudflare.com script allow",
    "michalrus.com clients6.google.com frame allow",
    "michalrus.com clients6.google.com script allow",
    "michalrus.com clients6.google.com xhr allow",
    "reddit.com reddit-uploaded-media.s3-accelerate.amazonaws.com xhr allow",
    "reddit.com redditmedia.com * allow",
    "reddit.com redditmedia.com frame allow",
    "reddit.com redditstatic.com * allow",
    "reddit.com v.redd.it * allow",
    "slack.com slack-edge.com * allow",
    "slack.com slack-edge.com script allow",
    "slack.com slack-msgs.com * allow",
    "stackoverflow.com ajax.googleapis.com script allow",
    "stackoverflow.com cdn.sstatic.net script allow",
    "thefreedictionary.com tfd.com script allow",
    "twitter.com twimg.com script allow",
    "twitter.com twimg.com xhr allow",
    "virtualpiano.net vpzone-xfojsycixf.netdna-ssl.com script allow",
    "youtube.com googlevideo.com * allow",
    "youtube.com ytimg.com * allow"
  ],
  "alwaysDetachLogger": false,
  "autoUpdate": true,
  "clearBrowserCache": true,
  "clearBrowserCacheAfter": 60,
  "cloudStorageEnabled": false,
  "collapseBlacklisted": true,
  "collapseBlocked": false,
  "colorBlindFriendly": false,
  "deleteCookies": true,
  "deleteUnusedSessionCookies": false,
  "deleteUnusedSessionCookiesAfter": 60,
  "deleteLocalStorage": true,
  "displayTextSize": "14px",
  "externalHostsFiles": [],
  "externalRecipeFiles": [],
  "iconBadgeEnabled": true,
  "maxLoggedRequests": 1000,
  "noTooltips": false,
  "popupCollapseAllDomains": false,
  "popupCollapseBlacklistedDomains": false,
  "popupScopeLevel": "domain",
  "processHyperlinkAuditing": true,
  "processReferer": false,
  "userHosts": {
    "enabled": false,
    "content": ""
  },
  "userRecipes": {
    "enabled": false,
    "content": ""
  }
}
