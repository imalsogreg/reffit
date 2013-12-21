<apply template="base">

  <div class="row">
    <!-- Left paper-roll column -->
    <div class="col-sm-8">
      <apply template="_paper_roll"/>
    </div>
    
    <!-- Right misc column (community stats, user feed, filtering) -->
    <div class="col-sm-4">
      
      <apply template="_filter_settings"/>

      <apply template="_community_stats"/>

      <apply template="userFeed"/>      
      
    </div>
    
  </div>
  
</apply>
