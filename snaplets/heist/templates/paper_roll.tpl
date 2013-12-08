<ignore>
  When called, this template will call _paper_roll.tpl with any bound splices,
  render it, and insert the result in base.tpl. (why?  who gets _'s?)
</ignore>

<apply template="base">
  <apply template="_paper_roll"/>
</apply>