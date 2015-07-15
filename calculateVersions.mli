open LocalFacts
open Reference

val collect_versions :
  Trace.objects -> bool -> facts_trace -> facts_trace
val calculate_versions: facts_tracefile -> facts_tracefile
