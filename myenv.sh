export GUILE_WARN_DEPRECATED=detailed
# export GUILE_AUTO_COMPILE=0
export ERACS_HOME=/Users/shane/School/uvm/CSYS-395-evolutionary-robotics/noweb-eracs
export DYLD_FALLBACK_LIBRARY_PATH=$ERACS_HOME/ctrnn/src/minimal-cognition/.libs:/opt/local/lib:/usr/local/lib:$ERACS_HOME/guile-mathematica/src/.libs

export GUILE_LOAD_PATH=.:$ERACS_HOME/emacsy/src:$ERACS_HOME/ctrnn/src/:$ERACS_HOME/guile-mathematica/src:

export PATH=.:`pwd`/bin:$PATH
export MYLOG_FILE=`pwd`/mylog.txt
export RUN_EXPR_DIR_ROOT=results
