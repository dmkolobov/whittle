(ns choreograph.core-test
  (:require [clojure.test :refer :all]
            [choreograph.core :refer :all])
  (:import (choreograph.core AnimationFrame)))

(def foo-lis
  {[:before :enter :foo] [[:a 1]]
   [:on     :enter :foo] [[:b 2]]
   [:after  :enter :foo] [[:c 3]]})

(def bar-lis
  {[:before :enter :bar] [[:d 4]]
   [:on     :enter :bar] [[:e 5]]
   [:after  :enter :bar] [[:f 6]]})

(def with-foo     (reduce add-listener {} foo-lis))
(def with-foo-bar (reduce add-listener with-foo bar-lis))

(deftest listener-test
  (testing "that add-listener builds the listener map properly. Applying
            add-listener with the same listener definition twice should
            return the listener map unchanged."

    (is (= with-foo
           {:foo {:enter {:before #{[[:a 1]]}
                          :on     #{[[:b 2]]}
                          :after  #{[[:c 3]]}}}}))

    (is (= with-foo-bar
           {:foo {:enter {:before #{[[:a 1]]}
                          :on     #{[[:b 2]]}
                          :after  #{[[:c 3]]}}}
            :bar {:enter {:before #{[[:d 4]]}
                          :on     #{[[:e 5]]}
                          :after  #{[[:f 6]]}}}}))))

(def triggered-by-foo
  {:before #{[[:a 1]]}
   :on     #{[[:b 2]]}
   :after  #{[[:c 3]]}})

(def triggered-by-bar
  {:before #{[[:d 4]]}
   :on     #{[[:e 5]]}
   :after  #{[[:f 6]]}})

(def triggered-by-both
  {:before #{[[:a 1]] [[:d 4]]}
   :on     #{[[:b 2]] [[:e 5]]}
   :after  #{[[:c 3]] [[:f 6]]}})

(deftest trigger-test
  (testing ""

    (is (= (trigger with-foo [:enter :foo])
           (trigger with-foo-bar [:enter :foo])
           (trigger-lifecycles with-foo [[:enter :foo]])
           (trigger-lifecycles with-foo-bar [[:enter :foo]])
           triggered-by-foo))

    (is(= (trigger with-foo-bar [:enter :bar])
          (trigger-lifecycles with-foo-bar [[:enter :bar]])
          triggered-by-bar))

    (is (= (trigger-lifecycles with-foo-bar [[:enter :foo] [:enter :bar]])
           triggered-by-both))))

(deftest stages-test
  (testing ""

    (is (= (trigger-stages with-foo-bar
                           [:before :on :after]
                           [[:enter :foo]
                            [:enter :bar]])

           [#{[[:a 1]] [[:d 4]]}
            #{[[:b 2]] [[:e 5]]}
            #{[[:c 3]] [[:f 6]]}]))))

(def test-frame
  (AnimationFrame. nil
                   0
                   {}
                   {:enter #{:foo :bar}}
                   with-foo-bar
                   [:before :on :after]))

(deftest fire-test
  (testing ""
    (is (= (:timeline (fire test-frame :enter [:foo]))
           (:timeline (-> test-frame
                          (fire :enter [:foo])
                          (fire :enter [:foo])))
           {[:a] [0 1]
            [:b] [1 2]
            [:c] [3 3]})))

  (testing ""
    (is (= (:timeline (-> test-frame
                          (fire :enter [:foo :bar])))
           (:timeline (-> test-frame
                          (fire :enter [:foo :bar])
                          (fire :enter [:foo])
                          (fire :enter [:bar])))
           {[:a] [0 1]
            [:d] [0 4]

            [:b] [4 2]
            [:e] [4 5]

            [:c] [9 3]
            [:f] [9 6]})))

  (testing ""
    (is (= (:timeline (-> test-frame
                          (fire :enter [:foo])
                          (fire :enter [:bar])))
           {[:a] [0 1]
            [:b] [1 2]
            [:c] [3 3]

            [:d] [6 4]
            [:e] [10 5]
            [:f] [15 6]}))

    (is (= (:timeline (-> test-frame
                          (fire :enter [:bar])
                          (fire :enter [:foo])))
           {[:d] [0 4]
            [:e] [4 5]
            [:f] [9 6]

            [:a] [15 1]
            [:b] [16 2]
            [:c] [18 3]}))))
