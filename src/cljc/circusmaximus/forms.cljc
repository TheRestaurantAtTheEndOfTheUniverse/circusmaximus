(ns circusmaximus.forms)

(def ^:const allowed-verb-combinations
  #{[:present :indicative :active]
    [:present :subjunctive :active]
    [:imperfect :indicative :active]
    [:imperfect :subjunctive :active]
    [:future :indicative :active]
    [:present :imperative :active]
    [:future :imperative :active]
    [:present :infinitive :active]
    [:present :participle :active]
    [:present :indicative :passive]
    [:present :subjunctive :passive]
    [:imperfect :indicative :passive]
    [:imperfect :subjunctive :passive]
    [:future :indicative :passive]
    [:present :imperative :passive]
    [:future :imperative :passive]
    [:present :infinitive :passive]
    [:future :participle :passive]
    [:perfect :indicative :active]
    [:perfect :subjunctive :active]
    [:plusquamperfect :indicative :active]
    [:plusquamperfect :subjunctive :active]
    [:futureperfect :indicative :active]
    [:perfect :infinitive :active]
    [:perfect :indicative :passive]
    [:perfect :subjunctive :passive]
    [:plusquamperfect :indicative :passive]
    [:plusquamperfect :subjunctive :passive]
    [:perfect :participle :passive]
    [:future :infinitive :active]
    [:present :supine :active]
    [:future :participle :active]})
