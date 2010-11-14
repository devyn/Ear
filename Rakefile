require 'rdiscount'

task :man => ["README.html"]

rule ".html" => [".md"] do |t|
  File.open( t.name, "w" ) do |f|

    f.write <<-EOF
<html><head>
<link rel="stylesheet" href="manual.css" type="text/css" />
</head><body>
#{RDiscount.new( File.read( t.source ) ).to_html}
</body></html>
EOF

  end
end
