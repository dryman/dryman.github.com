---
layout: post
title: "Jekyll/Octopress hacker news plugin"
date: 2012-04-05 14:49
comments: true
categories: Octopress
---

### Inspiration

I found out that some cool blogs have a cute *hacker news like button* on each of its
posts. 

{% img /images/hn_like.png %}

The service is [HN Like Button][hnlike], created by [shashyal][shashyal].
To use it you can use the generator like so:

{% img /images/gen_hn_like.png %}

However, we don't want to do that every time, right? So I decided to write a
small Jekyll plugin to generate the button automatically. Hope you can enjoy it!

### The hidden API of HNLike

Well...not really. The generated html snippet looks like this:
{% codeblock lang:html %}
<iframe frameborder="no" scrolling="no" height="50px" width="350px"
 src="http://hnlike.com/upvote.php?link=http%3A%2F%2Fdryman.github.com%2Fblog%2F2012%2F04%2F04%2Fjekyll-graphviz-plugin%2F&title=Jekyll%20Graphviz%20Plugin"
 >iframes not supported by your browser</iframe>
{% endcodeblock %}

Observe that in the `src` after the `upvote.php?` there are only two
key valure pairs:

1. `link=`**html link**
2. `&title=`**title text**

We just need to replace the **html link** and **title text** to our URL-escaped
string and it is done.

### Integrate with Jekyll

Jekyll/Octopress is designed to extend its functionality by users. For details
you can take a look at [Theming and Customization - Octopress][theme]. Follow
the instructions, I simply added a new html snippet in `source/_includes/`.

{% codeblock hn_like.html lang:html https://github.com/dryman/dryman.github.com/blob/src/source/_includes/hn_like.html link %}
{% raw %}
{% if site.hn_like_button %}
  <script type="text/javascript">
    (function(){
      var hn_like = document.createElement('iframe');
      hn_like.frameborder="no";
      hn_like.scrolling="no";
      hn_like.height="28px";
      hn_like.width="115px";
      hn_like.src = "http://hnlike.com/upvote.php?link="
                    + encodeURIComponent(document.location)
                    + "&title="
                    + encodeURIComponent("{{ page.title }}");
      hn_like.innerHTML="iframes not supported by your browser";
      var twitter = document.getElementsByClassName("twitter-share-button")[0];

      twitter.parentNode.insertBefore(
        hn_like,
        twitter
      );
    })();
  </script>
{% endif %}
{% endraw %}
{% endcodeblock %}

I modified the height and width otherwise it will be too big. Also I use jekyll
liquid helpers instead of `document.title` to form title text. Finally I use the
`twitter-share-button` element to position where I should insert the button in.

### Installation steps


1.  To extend the built in snippet, download [hn_like.html][plugin] into your 
    `source/_includes` directory
    
2.  Add one line {% raw %} `{% include hn_like.html %}` {% endraw %} into `source/_includes/after_footer.html`.

2.  Add

{% codeblock lang:yaml %}
# hacker news like button
hn_like_button: true
{% endcodeblock %}

into your `_config.yml`.



[hnlike]: http://hnlike.com/
[shashyal]: http://hacksandthoughts.posterous.com/
[plugin]: https://github.com/dryman/dryman.github.com/blob/src/source/_includes/hn_like.html
[theme]: http://octopress.org/docs/theme/template/
