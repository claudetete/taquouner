# Build wisitoken-bnf-generate.exe, for generating code from grammar files.
# 
# Assumes build.sh has run.
#
# Instead of using this, you should consider using the complete
# wisitoken development tree; see
# http://stephe-leake.org/ada/wisitoken.html

gprbuild -p -P wisitoken.gpr wisitoken-bnf-generate
