CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE IF NOT EXISTS users (
    user_id SERIAL PRIMARY KEY
  , user_email VARCHAR(64) UNIQUE NOT NULL
  , user_username VARCHAR(64) UNIQUE NOT NULL
  , user_password VARCHAR(60) NOT NULL
  , user_bio TEXT
  , user_image TEXT
  , user_createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , user_updatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS follows (
    fws_user_id INTEGER
  , fws_follows_user_id INTEGER
  , fws_createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , fws_updatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP

  , FOREIGN KEY (fws_user_id) REFERENCES users (user_id)
  , FOREIGN KEY (fws_follows_user_id) REFERENCES users (user_id)
  , UNIQUE (fws_user_id, fws_follows_user_id)
);

CREATE TABLE IF NOT EXISTS articles (
    article_id SERIAL PRIMARY KEY
  , article_slug VARCHAR(256) UNIQUE NOT NULL
  , article_title VARCHAR(256) NOT NULL
  , article_description VARCHAR(256) NOT NULL
  , article_body TEXT NOT NULL
  , article_createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , article_updatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , article_user_id INTEGER NOT NULL

  , FOREIGN KEY (article_user_id) REFERENCES users (user_id)
);

CREATE TABLE IF NOT EXISTS tags (
    tag_id SERIAL PRIMARY KEY
  , tag_text VARCHAR(32) UNIQUE
  , tag_createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , tag_updatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE IF NOT EXISTS tagged (
    tgd_article_id INTEGER
  , tgd_tag_id INTEGER
  , tgd_createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , tgd_updatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP

  , FOREIGN KEY (tgd_tag_id) REFERENCES tags (tag_id)
  , FOREIGN KEY (tgd_article_id) REFERENCES articles (article_id)
  , UNIQUE (tgd_tag_id, tgd_article_id)
);

CREATE TABLE IF NOT EXISTS favorited (
    favor_user_id INTEGER
  , favor_article_id INTEGER
  , favor_createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , favor_updatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP

  , FOREIGN KEY (favor_user_id) REFERENCES users (user_id)
  , FOREIGN KEY (favor_article_id) REFERENCES articles (article_id)
  , UNIQUE (favor_user_id, favor_article_id)
);

CREATE TABLE IF NOT EXISTS comments (
    comment_id SERIAL PRIMARY KEY
  , comment_uuid UUID UNIQUE DEFAULT uuid_generate_v4()
  , comment_body TEXT
  , comment_createdAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , comment_updatedAt TIMESTAMP DEFAULT CURRENT_TIMESTAMP
  , comment_article_id INTEGER
  , comment_user_id INTEGER

  , FOREIGN KEY (comment_article_id) REFERENCES articles (article_id)
  , FOREIGN KEY (comment_user_id) REFERENCES users (user_id)
);
