require 'rdiscount'
require 'nokogiri'

task :man => ["README.html"]

rule ".html" => [".md"] do |t|
  File.open( t.name, "w" ) do |f|

    c = Nokogiri::HTML( RDiscount.new( File.read( t.source ) ).to_html )
    h = c.css( 'h1' ).first.content

    n = Nokogiri::HTML.fragment( "<head><title></title><link rel='stylesheet' href='manual.css' type='text/css' /></head>" )
    n.at_css( "title" ).content = h

    c.at_css( "body" ).add_previous_sibling( n )

    c.write_to( f )

  end
end
