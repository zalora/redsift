// Models
Database = Backbone.Model.extend({});
Table = Backbone.Model.extend({});

// Collections
Databases = Backbone.Collection.extend({
  model: Database
});

// Views

var databases;


// Functions
function rebuildScroll() {
  $("#options").mCustomScrollbar("destroy");
  $("#options").mCustomScrollbar({
    theme:"light"
  }); 
}
function refreshTablesList(db_name) {
  var selDb = databases.findWhere({name:db_name}),
      selTbls = selDb.get("tables"),
      fTbls = [];
  $("#tablesList").empty();
  _.each(selTbls, function(tbl) {
    $("#tablesList").append("<li><a>" + tbl.get("name") + "</a></li>");
    fTbls.push({"table": tbl.get("name")})
  });
  rebuildScroll();

  start(fTbls);
}
function start(tables) {
    var $inputSearch = $('#searchTables'),
    $result = $('#tablesList'),
    searchTables = true,
    isCaseSensitive = false,
    fuse;
    function search() {
      var sterm = $inputSearch.val();
      var r = fuse.search(sterm);
      if( sterm === "" ) {
        $result.empty();
        $.each(tables, function() {
          $result.append('<li><a href="#">' + this.table + '</a</li>');
        });
        // need to rebuild the scroller
        $("#options").mCustomScrollbar("destroy");
        $("#options").mCustomScrollbar({
          theme:"light"
        }); // seems to remove focus on box?
      } else {
        $result.empty();
        $.each(r, function() {
          $result.append('<li><a href="#">' + this.table + '</a</li>');
        });
      }
    }
    function createFuse() {
      var keys = [];
      if (searchTables) {
        keys.push('table');
      }
      fuse = new Fuse(tables, {
        keys: keys,
        caseSensitive: isCaseSensitive
      });
    }
    $inputSearch.on('keyup', search);
    createFuse();
  }
// Start
$(document).ready(function() { 
  // fetch data from API
  
  // fetch data from local
  $.getJSON("tables.json", function(data) {
    var dbs = []
    _.each(data, function(db, db_name) {
      // todo: replace with view
      $("#databasesList").append("<li><a id='db__" + db_name + "'>" + db_name + "</a></li>");
      $('#db__' + db_name).click(function() {
        refreshTablesList($(this).text());
      });

      var database = new Database({
        name: db_name,
        tables: []
      }),
          tbls = [];
      _.each(db, function(tbl) {
        var table = new Table({
          name: tbl[0],
          is_view: tbl[1]
        });
        tbls.push(table);
      });
      database.set("tables", tbls);
      dbs.push(database);
    });
    databases = new Databases(dbs);
    rebuildScroll();
  });

  // initialize various plugins/3rd party
  $("#data-tbl").tablesorter(); 
  $("#options").mCustomScrollbar({
    theme:"light"
  }); // todo: call destroy and rebuild when tables refreshes
  $("#query_builder_toggle").click(function() {
    $("#query_builder").toggle( "fast", function() {
    // Animation complete.
    });
  });

  // codemirror
  var myCodeMirror = CodeMirror.fromTextArea(document.getElementById('sql_editor'), {
    lineNumbers: true,
    value: "-- your query here",
    mode: "text/x-sql"
  });

  // fuse: fuzzy search
  var tables = [{
    table: 'ampr',
    database: 'jurassicpark'
  },{
    table: 'browser',
    database: 'jurassicpark'
  },{
    table: 'cust_recommendation_vtd',
    database: 'jurassicpark'
  },{
    table: 'cvid',
    database: 'jurassicpark'
  },{
    table: 'devices',
    database: 'jurassicpark'
  },{
    table: 'eepr',
    database: 'jurassicpark'
  },{
    table: 'full_zpr',
    database: 'jurassicpark'
  },{
    table: 'idr_home_bestsellers',
    database: 'jurassicpark'
  },{
    table: 'idr_home_female_bestsellers',
    database: 'jurassicpark'
  },{
    table: 'idr_home_male_bestsellers',
    database: 'jurassicpark'
  },{
    table: 'idr_meta',
    database: 'jurassicpark'
  },{
    table: 'lpr',
    database: 'jurassicpark'
  },{
    table: 'orders_table',
    database: 'jurassicpark'
  },{
    table: 'osr',
    database: 'jurassicpark'
  },{
    table: 'st',
    database: 'jurassicpark'
  }];
  start(tables);
});