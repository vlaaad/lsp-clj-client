(ns io.github.vlaaad.lsp
  (:require [clojure.java.io :as io]
            [clojure.java.process :as process]
            [clojure.string :as string]
            [jsonista.core :as json])
  (:import [java.io InputStream OutputStream]
           [java.lang ProcessHandle]
           [java.net URI]
           [java.nio.charset StandardCharsets]
           [java.util.concurrent ArrayBlockingQueue BlockingQueue SynchronousQueue TimeUnit]))

(defn- read-ascii-line [^InputStream in]
  (let [sb (StringBuilder.)]
    (loop [carriage-return false]
      (let [ch (.read in)]
        (if (= -1 ch)
          (if (zero? (.length sb)) nil (.toString sb))
          (let [ch (char ch)]
            (.append sb ch)
            (cond
              (= ch \return) (recur true)
              (and carriage-return (= ch \newline)) (.substring sb 0 (- (.length sb) 2))
              :else (recur false))))))))

(defn- lsp-base [^InputStream in ^BlockingQueue server-in ^OutputStream out ^BlockingQueue server-out]
  (-> (Thread/ofVirtual)
      (.name "lsp-base-in")
      (.start
        #(loop []
           (when-some [headers (loop [acc {}]
                                 (when-let [line (read-ascii-line in)]
                                   (if (= "" line)
                                     acc
                                     (if-let [[_ field value] (re-matches #"^([^:]+):\s*(.+?)\s*$" line)]
                                       (recur (assoc acc (string/lower-case field) value))
                                       (throw (IllegalStateException. (str "Can't parse header: " line)))))))]
             (let [^String content-length (or (get headers "content-length")
                                              (throw (IllegalStateException. "Required header missing: Content-Length")))
                   len (Integer/valueOf content-length)
                   bytes (.readNBytes in len)]
               (if (= (alength bytes) len)
                 (do (.put server-in (json/read-value (String. bytes StandardCharsets/UTF_8) json/keyword-keys-object-mapper))
                     (recur))
                 (throw (IllegalStateException. "Couldn't read enough bytes"))))))))
  (-> (Thread/ofVirtual)
      (.name "lsp-base-out")
      (.start
        #(while true
           (let [^bytes message-bytes (json/write-value-as-bytes (.take server-out))]
             (doto out
               (.write (.getBytes (str "Content-Length: "
                                       (alength message-bytes)
                                       "\r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n")
                                  StandardCharsets/UTF_8))
               (.write message-bytes)
               (.flush)))))))

(defn- lsp-jsonrpc [^BlockingQueue client-in ^BlockingQueue server-in ^BlockingQueue server-out handlers]
  (let [in (SynchronousQueue.)]
    (-> (Thread/ofVirtual)
        (.name "lsp-jsonrpc-client")
        (.start #(while true (.put in [:client (.take client-in)]))))
    (-> (Thread/ofVirtual)
        (.name "lsp-jsonrpc-server")
        (.start #(while true (.put in [:server (.take server-in)]))))
    (-> (Thread/ofVirtual)
        (.name "lsp-jsonrpc")
        (.start
          #(loop [next-id 0
                  requests {}]
             (let [[src message] (.take in)]
               (case src
                 :client (let [out-message (cond-> {:jsonrpc "2.0"
                                                    :method (:method message)}
                                             (contains? message :params)
                                             (assoc :params (:params message)))]
                           (if-let [response-queue (:response message)]
                             (do
                               (.put server-out (assoc out-message :id next-id))
                               (recur (inc next-id) (assoc requests next-id response-queue)))
                             (do
                               (.put server-out out-message)
                               (recur next-id requests))))
                 :server (cond
                           ;; response?
                           (and (contains? message :id)
                                (or (contains? message :result)
                                    (contains? message :error)))
                           (let [id (:id message)
                                 ^BlockingQueue response-out (get requests id)]
                             (.put response-out message)
                             (recur next-id (dissoc requests id)))

                           ;; notification?
                           (and (contains? message :method)
                                (not (contains? message :id)))
                           (do
                             (when-let [handler (get handlers (:method message))]
                               (handler (:params message)))
                             (recur next-id requests))

                           ;; request?
                           (and (contains? message :method)
                                (contains? message :id))
                           (do
                             (.put
                               server-out
                               (try
                                 {:jsonrpc "2.0"
                                  :id (:id message)
                                  :result ((get handlers (:method message)) (:params message))}
                                 (catch Throwable e
                                   {:jsonrpc "2.0"
                                    :id (:id message)
                                    :error {:code -32603 :message (or (ex-message e) "Internal Error")}})))
                             (recur next-id requests))

                           :else
                           (do
                             (.put server-out {:jsonrpc "2.0" :id (:id message) :error {:code -32600 :message "Invalid Request"}})
                             (recur next-id requests))))))))))

(defn start!
  ([^Process process handlers]
   (start! (.getInputStream process) (.getOutputStream process) handlers))
  ([^InputStream in ^OutputStream out handlers]
   (let [client-in (ArrayBlockingQueue. 16)
         server-in (ArrayBlockingQueue. 16)
         server-out (ArrayBlockingQueue. 16)]
     (lsp-jsonrpc client-in server-in server-out handlers)
     (lsp-base in server-in out server-out)
     client-in)))

(defn notify!
  ([^BlockingQueue lsp method]
   (.put lsp {:method method}))
  ([^BlockingQueue lsp method params]
   (.put lsp {:method method :params params})))


(defn request!
  ([lsp method]
   (request! lsp method nil))
  ([^BlockingQueue lsp-client method params]
   (let [queue (SynchronousQueue.)]
     (.put lsp-client (cond-> {:method method :response queue} params (assoc :params params)))
     (let [m (.take queue)]
       (if-let [e (:error m)]
         (throw (ex-info (:message e) e))
         (:result m))))))

(defn- uri [path]
  (let [uri (.toURI (io/file path))]
    (URI. (.getScheme uri) "" (.getPath uri) nil)))

(defn lint [& {:keys [cmd path ext]}]
  (let [^Process process (apply process/start (if (string? cmd) [cmd] cmd))
        server (start! process {})]
    (try
      (tap> (request! server "initialize" {:processId (.pid (ProcessHandle/current))
                                           :rootUri (doto (uri path) tap>)
                                           :capabilities {:textDocument {:diagnostic {}}}}))
      (notify! server "initialized")
      (request! server "textDocument/diagnostic" {:textDocument {:uri (uri "test/lua/test.lua")}})
      (request! server "shutdown")
      (notify! server "exit")
      (finally
        (.waitFor process 2 TimeUnit/SECONDS)
        (when (.isAlive process)
          (.destroyForcibly process))))))

(lint :cmd "C:\\Users\\Vlaaad\\Downloads\\win32-x64\\win32-x64\\bin\\CodeFormatServer.exe"
      :path "test/lua")
