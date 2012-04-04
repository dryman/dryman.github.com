require 'digest/md5'

GRAPHVIZ_DIR = File.expand_path('../../source/images/graphviz', __FILE__)
FileUtils.mkdir_p(GRAPHVIZ_DIR)

module Jekyll
  class GraphvizBlock < Liquid::Block

    def initialize(tag_name, markup, tokens)
      super
    end

    def render(context)
      non_markdown = /(&amp|&lt|&nbsp|&quot|&gt|<\/p>|<\/h.>)/m
      code = super.join
      unless non_markdown.match(code)
        local_svg = File.join(GRAPHVIZ_DIR, "g-#{Digest::MD5.hexdigest(code)}.svg")
        web_svg = "/images/graphviz/g-#{Digest::MD5.hexdigest(code)}.svg"

        unless File.exist?(local_svg)
          puts local_svg
          puts code
          IO.popen("dot -Tsvg -o #{local_svg}", 'r+') do |pipe|
            pipe.puts(code)
            pipe.close_write
          end
        end
        "<img src='#{web_svg}'>"
      end 
    end
  end
end

Liquid::Template.register_tag('graphviz', Jekyll::GraphvizBlock)
